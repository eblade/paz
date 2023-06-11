s = '1bb773a60bd3f87aaddeadde000000001f97ef3b3226b335809acf3e9526153f52ba7143d12bcc281873af350fe9ad61a68968dac7781d221b1c5360a605d4b4'

for i in range(0, len(s), 2):
    print(f'0x{s[i:i+2]}, ', end='\n' if (i + 2) % 16 == 0 else '')
