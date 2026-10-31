"""OMRON 2JCIE-BU01 の最新値を、libusb を ctypes で直接叩いて一度だけ読む（読み取りのみ）。"""
import ctypes as C, struct, sys, time

lib = C.CDLL("/opt/homebrew/opt/libusb/lib/libusb-1.0.dylib")
lib.libusb_open_device_with_vid_pid.restype = C.c_void_p
lib.libusb_open_device_with_vid_pid.argtypes = [C.c_void_p, C.c_uint16, C.c_uint16]
for f in ("libusb_claim_interface", "libusb_release_interface"):
    getattr(lib, f).argtypes = [C.c_void_p, C.c_int]
lib.libusb_close.argtypes = [C.c_void_p]
lib.libusb_control_transfer.argtypes = [C.c_void_p, C.c_uint8, C.c_uint8, C.c_uint16, C.c_uint16,
                                        C.c_void_p, C.c_uint16, C.c_uint]
lib.libusb_bulk_transfer.argtypes = [C.c_void_p, C.c_uint8, C.c_void_p, C.c_int, C.POINTER(C.c_int), C.c_uint]

def crc16(data):
    crc = 0xFFFF
    for b in data:
        crc ^= b
        for _ in range(8):
            crc = (crc >> 1) ^ 0xA001 if crc & 1 else crc >> 1
    return crc

def frame(cmd, addr):
    payload = bytes([cmd]) + struct.pack("<H", addr)
    body = b"\x52\x42" + struct.pack("<H", len(payload) + 2) + payload
    return body + struct.pack("<H", crc16(body))

ctx = C.c_void_p()
assert lib.libusb_init(C.byref(ctx)) == 0
h = lib.libusb_open_device_with_vid_pid(ctx, 0x0590, 0x00D4)
if not h:
    sys.exit("RuntimeError: cannot open USB device 0590:00d4")
r = lib.libusb_claim_interface(h, 0)
if r != 0:
    sys.exit(f"RuntimeError: libusb_claim_interface(0) returned {r}")
OUT = 0x40
def ctl(req, val):
    r = lib.libusb_control_transfer(h, OUT, req, val, 1, None, 0, 1000)
    assert r >= 0, (req, r)
ctl(0, 0)        # reset
ctl(3, 26)       # 115200 baud（FT-X、3MHz 基準）
ctl(4, 0x0008)   # 8N1
ctl(2, 0)        # フロー制御なし
ctl(0, 1)        # 受信バッファを空に

req = frame(0x01, 0x5021)  # Latest data long
n = C.c_int()
buf = C.create_string_buffer(req)
assert lib.libusb_bulk_transfer(h, 0x02, buf, len(req), C.byref(n), 1000) == 0

data = b""
end = time.time() + 2
while time.time() < end:
    rb = C.create_string_buffer(512)
    r = lib.libusb_bulk_transfer(h, 0x81, rb, 512, C.byref(n), 200)
    if n.value > 2:
        pkt = rb.raw[:n.value]
        # 64 バイトごとの頭 2 バイトはモデムの状態
        for i in range(0, len(pkt), 64):
            data += pkt[i + 2:i + 64]
    if len(data) >= 4 and data[:2] == b"\x52\x42":
        need = 4 + struct.unpack("<H", data[2:4])[0]
        if len(data) >= need:
            break
lib.libusb_release_interface(h, 0)
lib.libusb_close(h)

if data[:2] != b"\x52\x42":
    sys.exit(f"ValueError: invalid sensor response header: {data[:40].hex()}")
ln = struct.unpack("<H", data[2:4])[0]
frm = data[:4 + ln]
ok = crc16(frm[:-2]) == struct.unpack("<H", frm[-2:])[0]
cmd, addr = frm[4], struct.unpack("<H", frm[5:7])[0]
d = frm[7:-2]
seq = d[0]
temp, hum, light = struct.unpack("<hHH", d[1:7])
press = struct.unpack("<I", d[7:11])[0]
noise, tvoc, co2, di, heat = struct.unpack("<HHHhh", d[11:21])
print(f"CRC {'ok' if ok else 'NG'} cmd={cmd:#x} addr={addr:#x} seq={seq}")
print(f"温度 {temp/100:.2f}℃ 湿度 {hum/100:.2f}% 照度 {light} lx 気圧 {press/1000:.1f} hPa "
      f"騒音 {noise/100:.1f} dB eTVOC {tvoc} ppb eCO2 {co2} ppm 不快指数 {di/100:.1f} 熱中症 {heat/100:.1f}")
