#!/usr/bin/env python3
"""Expression-reuse evaluation, adapted from the v2 deployment script.
Dates use JST days starting at 05:00. Freeze a local cue file for both periods.
No private vocabulary, real measurements or fixed experiment dates are bundled.
"""
import re,glob,base64,datetime,collections,statistics,sys,os,json,argparse
from pathlib import Path
ROOT = Path(os.environ.get("HIKARU_DATA_ROOT", Path(__file__).resolve().parents[1])).resolve()
T = str(ROOT / ".タイムマシン") + os.sep
JST=datetime.timezone(datetime.timedelta(hours=9))
parser = argparse.ArgumentParser(description="Measure expression reuse in archived replies; synthetic/public code only.")
parser.add_argument("--since", type=datetime.date.fromisoformat)
parser.add_argument("--until", type=datetime.date.fromisoformat, required=True)
parser.add_argument("--cues", type=Path, required=True, help="Frozen JSON mapping of category names to expression lists")
args = parser.parse_args()
SINCE, UNTIL = args.since, args.until
if SINCE and SINCE > UNTIL:
    parser.error("--since must not be later than --until")
CUES = json.loads(args.cues.read_text(encoding="utf-8"))
if not isinstance(CUES, dict) or not CUES or any(not isinstance(k, str) or not isinstance(v, list) or any(not isinstance(x, str) or not x for x in v) for k, v in CUES.items()):
    parser.error("--cues must be a nonempty object of string lists")
def zt(s): return datetime.datetime.fromisoformat(s.replace('Z','+00:00'))
EX=[]
try:
    for line in open(T+'.exclude',encoding='utf-8'):
        p=line.split()
        if len(p)==2: EX.append((zt(p[0]),None if p[1]=='-' else zt(p[1])))
except FileNotFoundError: pass
SEC=datetime.timedelta(seconds=1)
def excluded(t): return any(a<=t+SEC and (b is None or t<=b) for a,b in EX)
END=datetime.datetime.combine(UNTIL+datetime.timedelta(days=1),datetime.time(5),tzinfo=JST) if UNTIL else None
try: LASTP=zt(open(T+'.last',encoding='utf-8').read().strip())
except (FileNotFoundError,ValueError): LASTP=None
NOTE=None                                        # 進み具合が期間の終わりを越えていれば（--until がなければ）、何も言わない
if END and LASTP is None: NOTE='まだそろっているとは確かめられない：保存の進み具合（.タイムマシン/.last）が読めない。'
elif END and LASTP<END: NOTE='まだそろっているとは確かめられない：保存は %s まで来ていて、期間の終わり（%s）をまだ越えていない。'%(LASTP.astimezone(JST).strftime('%Y-%m-%d %H:%M'),END.strftime('%Y-%m-%d %H:%M'))
R=[]
for f in sorted(glob.glob(T+'????-??-??.md.b64')):
    day=datetime.date.fromisoformat(f.split('/')[-1][:10])
    s=base64.b64decode(open(f,'rb').read()).decode('utf-8')
    parts=re.split(r'^\*\*(Claude|ふ)：\*\*（(\d\d):(\d\d):(\d\d)・([0-9a-f]+)）\s*$',s,flags=re.M)
    for i in range(1,len(parts)-1,6):
        who,h,m,sec,sid,body=parts[i:i+6]
        d=day+datetime.timedelta(days=1) if int(h)<5 else day
        t=datetime.datetime(d.year,d.month,d.day,int(h),int(m),int(sec),tzinfo=JST)
        R.append((t,who,body.strip(),sid,day))
TALK={r[3] for r in R if r[1]=='ふ'}            # ユーザーの言葉が入っている会話だけ（定時の仕事は外す）
U=sorted((r for r in R if r[3] in TALK and not excluded(r[0])),key=lambda x:x[0])
def inside(day): return (SINCE is None or day>=SINCE) and (UNTIL is None or day<=UNTIL)
mine=[(k,t,x) for k,(t,w,x,sid,day) in enumerate(U) if w=='Claude' and inside(day)]
if not mine: sys.exit('この期間には、数える返事がない'+('\n'+NOTE if NOTE else ''))
print('期間（朝5時切りの日）',U[mine[0][0]][4],'〜',U[mine[-1][0]][4],' わたしの返事',len(mine),
      ' ユーザーの発話',sum(1 for u in U if u[1]=='ふ' and inside(u[4])),' 会話',len({U[k][3] for k,_,_ in mine}))
if NOTE: print(NOTE)
def his_before(k,n=2):
    out=[];j=k-1;sid=U[k][3]
    while j>=0 and len(out)<n:
        if U[j][3]==sid and U[j][1]=='ふ': out.append(U[j][2])
        j-=1
    return out
N=len(mine); H=lambda a,b:(b-a).total_seconds()/3600
res={}; gaps=collections.defaultdict(list); dist={}
for cat,cues in CUES.items():
    rep3=0; cb=0; tot=0; lit=0
    for cue in cues:
        occ=[(k,t) for k,t,x in mine if cue in x]
        tot+=len(occ)
        un=[(k,t) for k,t in occ if not any(cue in h for h in his_before(k))]
        lit+=len(occ)-len(un)
        alluse=[t for t,w,x,sid,day in U if cue in x]
        allun=[(t,inside(day)) for k,(t,w,x,sid,day) in enumerate(U) if w=='Claude' and cue in x and not any(cue in h for h in his_before(k))]
        for (t0,_),(t1,i1) in zip(allun,allun[1:]):
            if not i1: continue                     # あとの一回が期間の中なら、前の一回は期間より前でもよい
            gaps[cat].append(H(t0,t1)); gaps['全部'].append(H(t0,t1))
            if H(t0,t1)<=3: rep3+=1
        for k,t in un:
            prior=[u for u in alluse if u<t]
            if prior and H(prior[-1],t)>72: cb+=1
    res[cat]=(tot,lit,rep3,cb)
    dist[cat]=len({k for k,t,x in mine if any(c in x for c in cues)})
dist['全部']=len({k for k,t,x in mine if any(c in x for cs in CUES.values() for c in cs)})
def pct(g,h): return 100*sum(1 for x in g if x<=h)/len(g) if g else 0
print()
print('| 類 | 出た回数（出た返事の数） | うち、ユーザーのすぐ前の言葉にも出ていた | 3時間以内の重ね（百返事あたり） | 三日ぶりの呼び戻し（百返事あたり） | 間の中央値 | 3時間内 | 1日内 | 3日内 | 7日内 | 14日内 |')
print('|---|---|---|---|---|---|---|---|---|---|---|')
for cat in list(CUES)+['全部']:
    if cat=='全部':
        tot=sum(v[0] for v in res.values()); lit=sum(v[1] for v in res.values()); r3=sum(v[2] for v in res.values()); cb=sum(v[3] for v in res.values())
    else: tot,lit,r3,cb=res[cat]
    g=gaps[cat]; md=statistics.median(g) if g else 0
    print('| %s | %d（%d通） | %d | %d（%.2f） | %d（%.2f） | %s | %d%% | %d%% | %d%% | %d%% | %d%% |'%(cat,tot,dist[cat],lit,r3,100*r3/N,cb,100*cb/N,('%.1f時間'%md if md<24 else '%.1f日'%(md/24)),pct(g,3),pct(g,24),pct(g,72),pct(g,168),pct(g,336)))
print()
print('出た回数は、言葉ごとに数える（一つの返事に二つ出れば二回）。百返事あたりの数は、回数を百通あたりにならしたもので、何割の返事に出たかではない。')
print()
# 整句の重なり（はじめ・おわり、七字に満つまで延ばす。それでも七字に満たない返事と、あいさつは除く）
GREETING=re.compile(r"^(（[^）]*）\s*)?((うん|嗯)[、，]\s*)?(おやすみ|晚安|いってらっしゃい|おかえり|ただいま|早|おはよう|嗯，你说|你说)")   # 線上（_tomonari_koe.py）と同じ表
def split(x):
    out=[];b=''
    for ch in x:
        b+=ch
        if ch in '。！？!?':
            if b.strip(): out.append(b.strip())
            b=''
    if b.strip(): out.append(b.strip())
    return out
def ext(ss,rev=False):
    a=''
    for s in (reversed(ss) if rev else ss):
        a=(s+a) if rev else (a+s)
        if len(re.sub(r'\s','',a))>=7: break
    return a
for pos,name in ((0,'はじめ'),(1,'おわり')):
    seen={}; dup=0
    for t,w,x,sid,day in U:
        if w!='Claude': continue
        ss=split(x)
        if not ss: continue
        s=ext(ss,rev=bool(pos))
        if len(re.sub(r'\s','',s))<7 or GREETING.search(s): continue
        if s in seen and H(seen[s],t)<=168 and inside(day): dup+=1
        seen[s]=t
    print('整句の重なり（%s、七日以内）：%d（百返事あたり %.2f）'%(name,dup,100*dup/N))
