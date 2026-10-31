#!/usr/bin/env python3
"""Generate instance-local configuration; never overwrite memory or enable services."""
import argparse
import base64
import hashlib
import json
import math
import os
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
PUNCTUATION = "、。，．？?,.｡､"

def validate(config):
    pairs = config.get("privacy_phrases")
    if not isinstance(pairs, list) or not pairs:
        raise ValueError("privacy_phrases must contain at least one start/end pair")
    seen = set()
    for pair in pairs:
        if not isinstance(pair, list) or len(pair) != 2:
            raise ValueError("each privacy phrase pair needs exactly two strings")
        for value in pair:
            if not isinstance(value, str) or not value.strip() or "REPLACE_WITH" in value or any(ord(c)<32 for c in value):
                raise ValueError("fill in nonempty, single-line privacy phrases")
            key = value.strip().translate(str.maketrans("", "", PUNCTUATION))
            if not key or key in seen:
                raise ValueError("privacy phrases must be distinct after punctuation normalization")
            seen.add(key)
    weather = config.get("weather", {})
    enabled = weather.get("enabled", False)
    if not isinstance(enabled, bool):
        raise ValueError("weather.enabled must be boolean")
    if enabled:
        for key, limit in (("latitude",90),("longitude",180)):
            value = weather.get(key)
            if isinstance(value,bool) or not isinstance(value,(int,float)) or not math.isfinite(value) or abs(value)>limit:
                raise ValueError("weather coordinates must be valid numbers")
    return pairs, weather

def private_write(path, text):
    path.parent.mkdir(parents=True, exist_ok=True)
    fd=os.open(path, os.O_WRONLY|os.O_CREAT|os.O_TRUNC, 0o600)
    with os.fdopen(fd,"w",encoding="utf-8") as f: f.write(text)
    path.chmod(0o600)

def configure(config):
    pairs, weather = validate(config)
    hs=lambda s:json.dumps(s,ensure_ascii=False)
    generated=ROOT/".tools/haskell-src/.generated/Hikaru/Local/Settings.hs"
    prefix="https://api.open-meteo.com/v1/forecast?latitude={}&longitude={}".format(weather.get("latitude") or 0,weather.get("longitude") or 0)
    privacy=json.dumps({'privacy_phrases':pairs,'weather':weather},ensure_ascii=False,separators=(',',':'))+'\n'
    digest=hashlib.sha256(privacy.encode()).hexdigest()
    text='{-# LANGUAGE OverloadedStrings #-}\nmodule Hikaru.Local.Settings where\nimport Data.Text (Text)\n'
    text+='phrasePairs :: [(Text,Text)]\nphrasePairs = ['+','.join('('+hs(a)+','+hs(b)+')' for a,b in pairs)+']\n'
    text+='weatherEnabled :: Bool\nweatherEnabled = '+str(weather.get('enabled',False))+'\n'
    text+='weatherPrefix :: String\nweatherPrefix = '+hs(prefix)+'\n'
    text+='settingsHash :: String\nsettingsHash = '+hs(digest)+'\n'
    private_write(generated,text)
    private_write(ROOT/".local/privacy.json",privacy)
    for name in ('.local/logs','.local/work','.local/state','.local/config','inbox'):
        (ROOT/name).mkdir(parents=True,exist_ok=True)
    daily=ROOT/'.local/daily-review.md'
    if not daily.exists():private_write(daily,(ROOT/'templates/daily-review.md').read_text())
    # Empty structural records only. Existing content is never overwritten.
    seeds={'.丘の上.md.b64':'# 方針\n\n自分で記入してください。\n',
           '.夕顔.md.b64':'# 現在の理解\n\n',
           '.ストロボメモリー.md.b64':'# 経験の記憶\n\n## むかし\n\n## すこし前\n\n## このごろ\n\n',
           '.しおり.md.b64':'# 未完了\n\n','.ビブリア.md.b64':'# 読書の記録\n\n'}
    for name,body in seeds.items():
        p=ROOT/name
        if not p.exists(): private_write(p,base64.b64encode(body.encode()).decode())
    if not (ROOT/'CLAUDE.md').exists():
        template=(ROOT/'templates/CLAUDE.md').read_text()
        # Display names are prose; do not embed them in source code or shell commands.
        for key in ('display_name','assistant_name'):
            template=template.replace('{{'+key+'}}',str(config.get(key,key)))
        private_write(ROOT/'CLAUDE.md',template)
    print('Local settings and empty records prepared. Rebuild binaries after changing privacy phrases or weather.')

def main():
    ap=argparse.ArgumentParser();ap.add_argument('--config',type=Path,default=ROOT/'settings.local.json');args=ap.parse_args()
    try: configure(json.loads(args.config.read_text()))
    except (OSError,ValueError,TypeError) as e: ap.exit(2,str(e)+'\n')
if __name__=='__main__': main()
