#!/usr/bin/env python3
"""Build both binaries from source with a preinstalled GHC package database (macOS)."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys

ROOT=Path(__file__).resolve().parents[1]
SRC=ROOT/'.tools/haskell-src'
PKGS='base aeson http-client http-client-tls megaparsec parser-combinators bytestring containers directory filepath scientific text time vector base64-bytestring unix transformers crypton process random hashable unordered-containers'.split()

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--package-db',type=Path,required=True,help='GHC 9.10.3 package database with frozen dependencies installed')
    args=ap.parse_args()
    if sys.platform!='darwin':ap.error('Current C shims and sandbox require macOS')
    if subprocess.check_output(['ghc','--numeric-version'],text=True).strip()!='9.10.3':ap.error('Expected GHC 9.10.3')
    if not (SRC/'.generated/Hikaru/Local/Settings.hs').is_file():ap.error('Run scripts/configure.py first')
    build=(ROOT/'.build').resolve();build.mkdir(exist_ok=True)
    # Read existing dependencies; only this build directory is writable, no network.
    q=lambda v:json.dumps(str(v),ensure_ascii=False)
    policy='(version 1)(allow default)(deny network*)(deny file-write*)'+''.join('(allow file-write* (subpath '+q(p)+'))' for p in [build])+'(allow file-write* (literal "/dev/null"))'
    policy_file=build/'compile.sb';policy_file.write_text(policy)
    # Confirm that the isolated child starts; startup failures are not test evidence.
    subprocess.run(['/usr/bin/sandbox-exec','-f',str(policy_file),'/usr/bin/true'],check=True)
    for target,mainfile in [('hikaru-core','Main.hs'),('hikaru-owntime','OwnTimeMain.hs')]:
        out=build/target;out.mkdir(exist_ok=True)
        cfiles=[]
        for path in (SRC/'cbits').glob('*.c'):
            dest=out/path.name;shutil.copyfile(path,dest);cfiles.append(str(dest))
        cmd=['ghc','-XGHC2021','-O1','-threaded','-rtsopts','-hide-all-packages','-no-user-package-db','-package-env','-',
             '-package-db',str(args.package_db.resolve()),'-i'+str(SRC/'src'),'-i'+str(SRC/'.generated'),'-outputdir',str(out),'-tmpdir',str(out),
             '-o',str(out/target),'-licucore','-fdiagnostics-color=never']
        for pkg in PKGS:cmd+=['-package',pkg]
        cmd += [str(SRC/'app'/mainfile),*cfiles]
        with (out/'build.log').open('w') as log:
            result=subprocess.run(['/usr/bin/sandbox-exec','-f',str(policy_file),*cmd],stdout=log,stderr=subprocess.STDOUT,env=dict(os.environ,TMPDIR=str(out),xcrun_nocache="1"))
        if result.returncode:sys.exit('Build failed; inspect '+str(out/'build.log'))
        dest=ROOT/'.tools/bin'/target;dest.parent.mkdir(exist_ok=True);shutil.copy2(out/target,dest)
    for name in ['fuutou','mezame','ima','oshirase','ochibo','shirushi','choubo','koyomi']:
        dest=ROOT/'.tools/bin'/name
        if dest.is_symlink():dest.unlink()
        if not dest.exists():dest.symlink_to('hikaru-core')
    (ROOT/'.local/compiled-settings.sha256').write_text(hashlib.sha256((ROOT/'.local/privacy.json').read_bytes()).hexdigest()+'\n')
    print('Built hikaru-core and hikaru-owntime; no services installed or started.')
if __name__=='__main__':main()
