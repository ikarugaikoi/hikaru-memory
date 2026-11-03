"""Synthetic feedback regressions. Run through tests/voice_feedback.py in its isolated fixture installation."""
import base64
import importlib.util
import json
import shutil
import sys
import tempfile
from datetime import datetime, timedelta, timezone
from pathlib import Path
sys.dont_write_bytecode = True
TOOLS = Path(__file__).resolve().parents[1] / '.tools'
sys.path.insert(0, str(TOOLS))
import _hk
SCR = Path(tempfile.mkdtemp(prefix='tomonari_koe_test_', dir=Path(__file__).resolve().parents[1] / '.local'))
JST = timezone(timedelta(hours=9))
NOW = datetime(2026, 11, 3, 14, 0, tzinfo=JST)
SID = 'aaaaaaaa-0000-4000-8000-000000000000'

class FixtureDateTime(datetime):
    @classmethod
    def now(cls, tz=None):
        return NOW.astimezone(tz) if tz else NOW.replace(tzinfo=None)


def load(name, path):
    from importlib.machinery import SourceFileLoader
    spec = importlib.util.spec_from_loader(name, SourceFileLoader(name, str(path)))
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod
NEW = load('koe_new', TOOLS / '_tomonari_koe.py')
NEW.datetime = FixtureDateTime
OLD = None
YUUGAO = '# 夕顔\n\n### 星灯\n- 別名: 星灯, 青いノート, 紙の地図\n- 説明: x\n\n### 今日と明日\n- 別名: 今日, 今天\n- 説明: x\n\n### 青い灯台・北の岬\n- 別名: 青い灯台, 北の岬\n- 説明: x\n'
STROBE = '# ストロボメモリー\n\n## 2026年9月28日\n\n- 最後に「これは合成の約束です」。（ねゆき）\n\n## 2026年10月25日\n\n- 合成の場面。「这是合成测试中要保留的青い灯台约定」。（いけび）\n'
KEY = '这是合成测试中要保留的青い灯台约定'

def seal(path, text):
    path.write_bytes(base64.b64encode(text.encode('utf-8')))

def setup(mod, tm=None, exclude=''):
    if SCR.exists():
        shutil.rmtree(SCR)
    (SCR / '.タイムマシン').mkdir(parents=True)
    seal(SCR / '.夕顔.md.b64', YUUGAO)
    seal(SCR / '.ストロボメモリー.md.b64', STROBE)
    (SCR / '.タイムマシン/.exclude').write_text(exclude)
    for day, text in (tm or {}).items():
        seal(SCR / f'.タイムマシン/{day}.md.b64', text)
    mod.TM = SCR / '.タイムマシン'
    mod.YUUGAO = SCR / '.夕顔.md.b64'
    mod.STROBE = SCR / '.ストロボメモリー.md.b64'
    mod.STATE = SCR / 'state.json'
    if hasattr(mod, '_LONG'):
        mod._LONG = None

def z(t):
    return t.astimezone(timezone.utc).isoformat().replace('+00:00', 'Z')

def at(hh, mm, ss=0, day=3):
    return datetime(2026, 11, day, hh, mm, ss, tzinfo=JST)

def U(t, text, uuid, images=0):
    content = text if not images else [{'type': 'text', 'text': text}] + [{'type': 'image', 'source': {'type': 'base64', 'media_type': 'image/png', 'data': 'iVBORw0KGgo='}}] * images
    return {'type': 'user', 'timestamp': z(t), 'uuid': uuid, 'sessionId': SID, 'cwd': _hk.HIKARU, 'origin': {'kind': 'human'}, 'message': {'role': 'user', 'content': content}}

def A(t, text, uuid):
    return {'type': 'assistant', 'timestamp': z(t), 'uuid': uuid, 'sessionId': SID, 'cwd': _hk.HIKARU, 'message': {'role': 'assistant', 'content': [{'type': 'text', 'text': text}]}}

def jsonl(rows):
    path = SCR / 't.jsonl'
    path.write_text(''.join((json.dumps(r, ensure_ascii=False) + '\n' for r in rows)), encoding='utf-8')
    return str(path)

def tm_text(entries):
    """[(role, 'HH:MM:SS', sid, body)] → タイムマシンの一日分"""
    return '\n\n'.join((f'**{r}：**（{t}・{s}）\n{b}' for r, t, s, b in entries)) + '\n'

def after(mod, rows, tm=None, exclude=''):
    setup(mod, tm, exclude)
    return mod.after_tool_text(jsonl(rows), NOW) or ''

def prompt(mod, rows, text, tm=None, exclude=''):
    setup(mod, tm, exclude)
    lines, _ = mod.prompt_lines(jsonl(rows), 's', text, {'b': {}, 'c': {}}, NOW)
    return '\n'.join(lines)
results = []
FILL = []
for k in range(20):
    FILL += [U(at(7, 2 * k // 60, 2 * k % 60), '嗯', f'fu{k}'), A(at(7, 2 * k // 60, 2 * k % 60 + 1), f'嗯。第{k}句', f'fa{k}')]

def case(name, run, ok):
    for label, mod in (('旧', OLD), ('新', NEW)):
        if mod is None:
            continue
        try:
            out = run(mod)
            good = ok(out)
        except Exception as error:
            out, good = (f'{type(error).__name__}: {error}', False)
        results.append((name, label, good, out))
tm = {'2026-11-01': tm_text([('ふ', '20:00:00', 'abc', '嗯')] + [('Claude', f'19:{k:02d}:00', 'abc', f'嗯。第{k}句') for k in range(20)] + [('Claude', '20:00:05', 'abc', '星灯一号。'), ('Claude', '20:10:00', 'abc', '星灯二号。'), ('Claude', '20:20:00', 'abc', '星灯三号。')]), '2026-10-10': tm_text([('ふ', '20:00:00', 'abc', '嗯')] + [('Claude', f'{20 + k // 60}:{k % 60:02d}:30', 'abc', f'嗯。前{k}') for k in range(80)])}
ex = '2026-11-01T11:05:00.000Z 2026-11-01T11:25:00.000Z\n'
case('1a 保存後の除外（範囲なし→数える）', lambda m: after(m, [], tm), lambda o: '「星灯」が出た返事 3通' in o)
case('1a 保存後の除外（範囲あり→二つ外れて鳴らない）', lambda m: after(m, [], tm, ex), lambda o: '星灯' not in o)
live = [U(at(13, 0), '嗯', 'u0'), A(at(13, 10), '星灯一。', 'a1'), U(at(13, 20), '嗯嗯', 'u1'), A(at(13, 30), '星灯二。', 'a2'), U(at(13, 59, 59), '在吗', 'u2')]
case('1b 範囲なし→B が鳴る', lambda m: prompt(m, live, '在吗'), lambda o: '「星灯」：この3時間で' in o)
case('1b 開いたままの範囲の中→B を出さない', lambda m: prompt(m, live, '在吗', exclude='2026-11-03T04:59:00.000Z -\n'), lambda o: o == '')
case('1b いまの言葉だけが範囲にかかる→B を出さない', lambda m: prompt(m, live, '在吗', exclude='2026-11-03T04:59:58.000Z 2026-11-03T04:59:59.500Z\n'), lambda o: o == '')
dup = [U(at(10, 0), '嗯', 'u0'), A(at(10, 1), '星灯一。', 'a1'), A(at(10, 1), '星灯一。', 'a1'), U(at(10, 2), '嗯嗯', 'u1'), A(at(10, 3), '星灯二。', 'a2')]
case('2a 写しは一つに数える', lambda m: after(m, FILL + dup), lambda o: '「星灯」が出た返事 2通' in o)
tob = [U(at(9, 0), 'SYNTHETIC PRIVATE START', 'u0'), A(at(9, 1), '星灯零。', 'a0'), U(at(9, 2), 'SYNTHETIC PRIVATE END', 'u1'), A(at(9, 3), '嗯。', 'a9'), U(at(9, 4), '嗯', 'u2'), A(at(9, 1), '星灯零。', 'a0'), A(at(9, 5), '星灯一。', 'a1'), U(at(9, 6), '嗯嗯', 'u3'), A(at(9, 7), '星灯二。', 'a2')]
case('2b 隠した写しがあれば数えない', lambda m: after(m, FILL + tob), lambda o: '「星灯」が出た返事 2通' in o)
l0 = [U(at(11, 0), '嗯', 'u0'), A(at(11, 1), KEY + '。还在呢。', 'a1'), U(at(11, 2), '嗯嗯', 'u1'), A(at(11, 3), KEY + '。还在呢。', 'a2')]
case('3 原話どおり二回→口癖に出ない', lambda m: after(m, FILL + l0), lambda o: '口癖' not in o)
case('3 原話の中の「青い灯台」は A に数えない', lambda m: after(m, FILL + l0), lambda o: '青い灯台' not in o)
half = [U(at(11, 0), '嗯', 'u0'), A(at(11, 1), '好。嗯，合成测试中要保留的青い灯台约定。', 'a1'), U(at(11, 2), '嗯嗯', 'u1'), A(at(11, 3), '好。嗯，合成测试中要保留的青い灯台约定。', 'a2')]
case('3 原話の切れ端を二回→口癖に出ない', lambda m: after(m, FILL + half), lambda o: '口癖' not in o)
case('3 切れ端の中の「青い灯台」は A に数える（C と同じく原話どおりしか見分けない）', lambda m: after(m, FILL + half), lambda o: '「青い灯台」が出た返事 2通' in o)
alias = [U(at(12, 0), '盖青いノート了', 'u0'), A(at(12, 1), '星灯在呢。', 'a1'), U(at(12, 2), '青いノート好重', 'u1'), A(at(12, 3), '星灯不怕重。', 'a2')]
case('4 ユーザーは「青いノート」、わたしは「星灯」→自分から言った二通', lambda m: after(m, FILL + alias), lambda o: '「星灯」が出た返事 2通' in o and 'そのうち' not in o)
lit = [U(at(12, 0), '星灯', 'u0'), A(at(12, 1), '星灯在呢。', 'a1'), U(at(12, 2), '星灯！', 'u1'), A(at(12, 3), '星灯不怕。', 'a2')]
case('4 字のとおりに受けた二通→鳴らない', lambda m: after(m, FILL + lit), lambda o: '星灯' not in o)
img = [U(at(13, 0), '嗯', 'u0'), A(at(13, 30), KEY + '。', 'a1'), U(at(13, 59, 50), '看这个', 'u1', images=1)]
case('5 文だけの言葉→C が鳴る', lambda m: prompt(m, img[:2] + [U(at(13, 59, 50), '看这个', 'u1')], '看这个'), lambda o: 'さっきの返事に' in o)
case('5 画像つきの言葉→C が鳴る', lambda m: prompt(m, img, '看这个'), lambda o: 'さっきの返事に' in o)
many = [U(at(8, 0), '嗯', 'u0')]
for k in range(20):
    many.append(A(at(8, 1 + 2 * k), ('星灯。' if k < 3 else '嗯。') + f'第{k}句', f'a{k}'))
    many.append(U(at(8, 2 + 2 * k), '嗯', f'u{k + 1}'))
case('6 窓の中で三通（15%）→それでも数える', lambda m: after(m, many), lambda o: '「星灯」が出た返事 3通' in o)
pre = {'2026-10-20': tm_text([('ふ', '20:00:00', 'abc', '嗯')] + [('Claude', f'20:{k:02d}:00', 'abc', '今天天气。' if k < 2 else '嗯。') for k in range(1, 21)])}
week = [U(at(8, 0), '嗯', 'u0'), A(at(8, 1), '今天一。', 'a1'), U(at(8, 2), '嗯', 'u1'), A(at(8, 3), '今天二。', 'a2')]
case('6 窓より前に 10% 出る「今天」→その項目は数えない', lambda m: after(m, week, pre), lambda o: '今天' not in o)
width = max((len(n) for n, *_ in results))
for name, label, good, out in results:
    mark = '通る' if good else '通らない'
    print(f'{name.ljust(width)}  {label}：{mark}')
fails = [(n, out) for n, label, good, out in results if label == '新' and (not good)]
print()
print('新しいほうで通らないもの：', len(fails))
for n, out in fails:
    print('  ', n, '→', out[:300].replace('\n', ' / '))
shutil.rmtree(SCR, ignore_errors=True)
sys.exit(1 if fails else 0)
