#!/usr/bin/env python3
"""Create a source-only archive from an explicit reviewable allowlist."""
import argparse
import hashlib
from pathlib import Path
import zipfile

ROOT=Path(__file__).resolve().parents[1]
FORBIDDEN={'.git','.local','.generated','.build','dist-newstyle','__pycache__','index','bak','locks','bin','.venv'}

def paths():
    items=[]
    for name in (ROOT/'release-files.txt').read_text().splitlines():
        if not name or name.startswith('#'):continue
        p=Path(name)
        if p.is_absolute() or '..' in p.parts or set(p.parts)&FORBIDDEN or p.suffix in ('.b64','.pyc','.o','.hi','.sock') or name in ('settings.local.json','CLAUDE.md'):
            raise ValueError('Non-public path in allowlist: '+name)
        f=ROOT/p
        if not f.is_file() or f.is_symlink() or any(parent.is_symlink() for parent in f.parents if parent!=ROOT):
            raise ValueError('Missing file or symlink: '+name)
        f.read_bytes().decode('utf-8')  # Source bundle contains text only.
        items.append(name)
    if len(items)!=len(set(items)):raise ValueError('Duplicate public path')
    return sorted(items)

def main():
    ap=argparse.ArgumentParser();ap.add_argument('output',type=Path);args=ap.parse_args()
    output=args.output.resolve()
    if output==ROOT or ROOT in output.parents:ap.error('Write the archive outside the source repository')
    files=paths();output.parent.mkdir(parents=True,exist_ok=True)
    with zipfile.ZipFile(output,'w',compression=zipfile.ZIP_DEFLATED) as z:
        for name in files:
            info=zipfile.ZipInfo('hikaru-memory/'+name,date_time=(2026,10,31,0,0,0))
            info.compress_type=zipfile.ZIP_DEFLATED;info.external_attr=0o100644<<16
            z.writestr(info,(ROOT/name).read_bytes())
    digest=hashlib.sha256(output.read_bytes()).hexdigest()
    output.with_suffix(output.suffix+'.sha256').write_text(digest+'  '+output.name+'\n')
    print(f'{len(files)} public files; {output.stat().st_size} bytes; sha256 {digest}')
if __name__=='__main__':main()
