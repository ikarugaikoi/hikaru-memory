#!/usr/bin/env python3
"""Synthetic integration checks, relocated installation, no real logs or network."""
import ast
import argparse
import base64
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile

SOURCE=Path(__file__).resolve().parents[1]

def main():
    ap=argparse.ArgumentParser();ap.add_argument('--package-db',type=Path);args=ap.parse_args()
    if sys.platform!='darwin':sys.exit('Synthetic sandbox currently requires macOS')
    for p in SOURCE.rglob('*.py'):
        if '.build' not in p.parts:ast.parse(p.read_text(),filename=str(p))
    with tempfile.TemporaryDirectory(prefix='hikaru-release-test-') as td:
        work=Path(td).resolve();root=work/'relocated installation';root.mkdir()
        shutil.copytree(SOURCE/'.tools',root/'.tools',ignore=shutil.ignore_patterns('.generated','index','bak','locks','__pycache__','dist-newstyle','.build'))
        shutil.copytree(SOURCE/'scripts',root/'scripts')
        shutil.copytree(SOURCE/'templates',root/'templates')
        cfg=json.loads((SOURCE/'settings.example.json').read_text())
        cfg['display_name']='Synthetic User';cfg['privacy_phrases']=[['SYNTHETIC START','SYNTHETIC END']]
        # Binary and Python must use the same configured phrases; test binary's compiled settings.
        cfg['privacy_phrases']=[['SYNTHETIC PRIVATE START','SYNTHETIC PRIVATE END']]
        (root/'settings.local.json').write_text(json.dumps(cfg))
        env=dict(os.environ,TMPDIR=str(work),PYTHONDONTWRITEBYTECODE='1',HIKARU_EMBED_SERVER='0')
        for key in list(env):
            if key.startswith(('HIKARU_','CLAUDE_CODE_','MIZUKAGAMI_')):env.pop(key,None)
        env['HIKARU_EMBED_SERVER']='0'
        q=lambda x:json.dumps(str(x),ensure_ascii=False)
        # Prevent accidental client-log reads; only synthetic installation is writable.
        policy='(version 1)(allow default)(deny network*)(deny file-write*)'+f'(allow file-write* (subpath {q(work)}) (literal "/dev/null"))'+f'(deny file-read-data (subpath {q(Path.home()/".claude")}))'
        sb=work/'test.sb';sb.write_text(policy)
        subprocess.run(['/usr/bin/sandbox-exec','-f',str(sb),'/usr/bin/true'],check=True,env=env)
        checks=[]
        def run(args,code=0):
            p=subprocess.run(['/usr/bin/sandbox-exec','-f',str(sb),sys.executable,*args],cwd=root,env=env,text=True,capture_output=True)
            if code is not None and p.returncode!=code:raise AssertionError((args,p.returncode,p.stdout,p.stderr))
            return p
        def tool(*args,code=0):return run(['scripts/hikaru',*args],code)
        def ok(name):checks.append(name)
        run(['scripts/configure.py']);ok('fresh local setup')
        if args.package_db:
            subprocess.run([sys.executable,str(root/'scripts/build.py'),'--package-db',str(args.package_db.resolve())],check=True,env=env)
            ok('build in relocated synthetic installation')
        elif not (root/'.tools/bin/hikaru-core').is_file():
            sys.exit('Pass --package-db to compile synthetic binaries')
        tool('fuutou','status');ok('relocated core status')
        assert '現在の理解' in tool('fuutou','show','yuugao').stdout;ok('read empty current-understanding record')
        patch=root/'.local/work/new.txt';patch.write_text('### 2026年10月30日\n\n青い箱を見た。\n')
        tool('fuutou','append','strobe',str(patch));assert '青い箱' in tool('fuutou','show','strobe').stdout;ok('language-checked append and read')
        patch.write_text('賠った。\n');before=(root/'.ストロボメモリー.md.b64').read_bytes()
        assert tool('fuutou','append','strobe',str(patch),code=None).returncode!=0
        assert before==(root/'.ストロボメモリー.md.b64').read_bytes();ok('language finding blocks mutation')
        assert tool('fuutou','show','tm:../outside',code=None).returncode!=0;ok('record path traversal rejected')
        draft=Path(tool('fuutou','open','yuugao').stdout.strip());draft.write_text(draft.read_text()+'補足。\n')
        dst=root/'.夕顔.md.b64';dst.write_bytes(base64.b64encode('別の変更。\n'.encode()))
        assert tool('fuutou','close','yuugao',code=None).returncode!=0;ok('concurrent edit rejected')
        dst.write_bytes(base64.b64encode('# 現在の理解\n'.encode()))
        tool('fuutou','clean');ok('temporary draft cleanup')
        tool('fuutou','fade');ok('fade plan on synthetic records')
        # Make a client-format transcript entirely from generated text.
        rows=[]
        for i,(role,text) in enumerate([('user','PUBLIC BEFORE'),('user','SYNTHETIC PRIVATE START'),('assistant','PRIVATE SECRET'),('user','SYNTHETIC PRIVATE END'),('user','PUBLIC AFTER')]):
            rows.append({'type':role,'origin':{'kind':'human'},'uuid':f'synthetic-{i}','sessionId':'synthetic-session','timestamp':f'2026-10-30T10:0{i}:00Z','cwd':str(root),'message':{'role':role,'content':text},'parentUuid':None if i==0 else f'synthetic-{i-1}'})
        log=root/'.local/logs/synthetic-session.jsonl';log.write_text(''.join(json.dumps(x)+'\n' for x in rows))
        tool('ochibo','--dry-run');ok('collect dry run on synthetic transcript')
        tool('ochibo');archived=''.join(base64.b64decode(p.read_bytes()).decode() for p in (root/'.タイムマシン').glob('*.md.b64'))
        assert 'PUBLIC BEFORE' in archived and 'PUBLIC AFTER' in archived and 'PRIVATE SECRET' not in archived;ok('compiled exclusion phrases omit private text')
        probe='import sys;sys.path.insert(0,".tools");import _hk;assert _hk.classify_phrase("SYNTHETIC PRIVATE START")=="start";assert _hk.classify_phrase("mentions SYNTHETIC PRIVATE START")==None;import _kotoba;assert _kotoba.retired()[1];import _ayame;import doko;import taguri_hook;print("OK")'
        run(['-c',probe]);ok('Python configuration, helpers and phrase boundaries')
        result=tool('omoide','PUBLIC','--mode','lexical');assert 'PUBLIC' in result.stdout;ok('lexical retrieval of synthetic archive')
        tool('_koyori','--rebuild');ok('rebuild catalog without embedding model')
        tool('owntime','status');ok('own-time status without launching model')
        configfile=root/'.local/privacy.json';configfile.write_text(configfile.read_text()+' ')
        assert tool('fuutou','status',code=None).returncode!=0
        assert run(['-c','import sys;sys.path.insert(0,".tools");import _hk'],code=None).returncode!=0
        ok('changed local settings rejected by both runtimes until rebuilt')
        print(json.dumps({'passed':len(checks),'checks':checks,'fixtures':'synthetic only','network':'denied','services_started':False},ensure_ascii=False,indent=2))
if __name__=='__main__':main()
