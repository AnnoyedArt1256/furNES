from chipchune.furnace.module import FurnaceModule
from chipchune.furnace.data_types import InsFeatureMacro, InsFeatureDPCMMap, InsFeatureAmiga, InsFeatureFM
from chipchune.furnace.enums import MacroCode
from chipchune.furnace.enums import MacroItem
from chipchune.furnace.enums import InstrumentType
import sys

subsong = 0

print(sys.argv)
module = FurnaceModule(sys.argv[1])

speed_type = len(module.subsongs[subsong].speed_pattern)

notes = ["C_","Cs","D_","Ds","E_","F_","Fs","G_","Gs","A_","As","B_"]

def conv_pattern(pattern):
    out = []
    oldtemp = [0,0]
    r = 0
    for row in pattern.data:
        has03xx = 0
        for l in row.effects:
            k = list(l)
            if k[0] == 0x03 and k[1] > 0:
                has03xx = 1
                break

        bit = 0
        temp = []
        notnote = 0
        if str(row.note) == "OFF_REL":
            notnote = 1
            temp.append(0x82)
        elif str(row.note) == "REL":
            notnote = 1
            temp.append(0x82)
        elif str(row.note) == "OFF":
            notnote = 1
            temp.append(0x81)
        elif str(row.note) == "__" or (has03xx == 1):
            if has03xx == 0:
                notnote = 1
            temp.append(0x80)
        else:
            temp.append(max(min(notes.index(str(row.note))+(row.octave*12),127),0))
        if row.instrument != 65535:
            bit |= 1
            temp.append(row.instrument)
        if row.volume != 65535:
            bit |= 2
            temp.append(row.volume)
        hasEffect = [-1,-1]
        t = temp
        temp = []
        for l in row.effects:
            k = list(l)
            if k[1] == 65535:
                k[1] = 0
            if k[0] == 0xED:
                temp.extend([0xFD, 0xED, k[1]])
                break

        has0Dxx = -1
        for l in row.effects:
            k = list(l)
            if k[1] == 65535:
                k[1] = 0
            if k[0] == 0x00:
                temp.extend([0xFD, 0x00, k[1]])
                continue
            if k[0] == 0xD:
                has0Dxx = k[1]
                continue
            if k[0] == 0x10:
                temp.extend([0xFE, k[1]])
                continue
            if k[0] == 0x11:
                temp.extend([0xFD, 0x11, k[1]])
                continue
            if k[0] == 0x12:
                temp.extend([0xFE, k[1]])
                continue
            if (k[0] == 0x09 or k[0] == 0x0F) and (speed_type == 1):
                temp.extend([0xFD, 0x0F, k[1]])
                temp.extend([0xFD, 0x09, k[1]])
                continue
            if k[0] == 0x0F and (speed_type == 2):
                temp.extend([0xFD, 0x0F, k[1]])
                continue
            if k[0] == 0x09 and (speed_type == 2):
                temp.extend([0xFD, 0x09, k[1]])
                continue
            if k[0] == 0x01:
                temp.extend([0xFD, 0x01, k[1], 88])
                continue
            if k[0] == 0x02:
                temp.extend([0xFD, 0x02, k[1], 7])
                continue
            if k[0] == 0x03 and k[1] == 0:
                temp.extend([0xFD, 0x02, 0, 7])
                continue
            if k[0] == 0x03 and k[1] > 0:
                temp.extend([0xFD, 0x03, k[1], max(min(notes.index(str(row.note))+(row.octave*12),127),0)])
                continue
            if k[0] == 0x04:
                temp.extend([0xFD, 0x04, k[1]])
                continue
            if k[0] == 0x0A:
                if k[1] == 0:
                    temp.extend([0xFD, 0x0A, 0])
                elif k[1] < 0x10:
                    temp.extend([0xFD, 0x0A, (k[1]|0x10)<<2])
                else:
                    temp.extend([0xFD, 0x0A, (k[1]>>4)<<2])
                continue
            if k[0] == 0xE1:
                temp.extend([0xFD, 0xE1, k[1]>>4, k[1]&15])
                continue
            if k[0] == 0xE2:
                temp.extend([0xFD, 0xE2, k[1]>>4, k[1]&15])
                continue
            if k[0] == 0xE5:
                temp.extend([0xFD, 0xE5, k[1]])
                continue
            if k[0] == 0xEC:
                temp.extend([0xFD, 0xEC, k[1]])
                continue
        temp.append(bit)
        temp.extend(t)
        if (oldtemp != temp) or (out[-1] > 120) or (len(out) == 0):
            out.extend(temp)
            out.append(0)
        oldtemp = temp
        if has0Dxx > -1:
            out[-1] += 1
            out.extend([0xFF, has0Dxx])
            return out
        out[-1] += 1
        r += 1
    out.extend([0xFF, 0])
    return out

f = open("nsf/song.asm","w")

relV = []
relA = []
relD = []

f.write("ticks_init:")
f.write(".byte ")
if speed_type == 1:
    f.write(str(module.subsongs[subsong].speed_pattern[0])+", ")
    f.write(str(module.subsongs[subsong].speed_pattern[0])+"\n")
elif speed_type == 2:
    f.write(str(module.subsongs[subsong].speed_pattern[0])+", ")
    f.write(str(module.subsongs[subsong].speed_pattern[1])+"\n")

f.write("insVL:\n")
f.write(".lobytes ")
for i in range(len(module.instruments)):
    f.write("ins"+str(i)+"V")
    if i == len(module.instruments)-1:
        f.write("\n")
    else:
        f.write(", ")
f.write("insVH:\n")
f.write(".hibytes ")
for i in range(len(module.instruments)):
    f.write("ins"+str(i)+"V")
    if i == len(module.instruments)-1:
        f.write("\n")
    else:
        f.write(", ")

f.write("insAL:\n")
f.write(".lobytes ")
for i in range(len(module.instruments)):
    f.write("ins"+str(i)+"A")
    if i == len(module.instruments)-1:
        f.write("\n")
    else:
        f.write(", ")
f.write("insAH:\n")
f.write(".hibytes ")
for i in range(len(module.instruments)):
    f.write("ins"+str(i)+"A")
    if i == len(module.instruments)-1:
        f.write("\n")
    else:
        f.write(", ")

f.write("insDL:\n")
f.write(".lobytes ")
for i in range(len(module.instruments)):
    f.write("ins"+str(i)+"D")
    if i == len(module.instruments)-1:
        f.write("\n")
    else:
        f.write(", ")
f.write("insDH:\n")
f.write(".hibytes ")
for i in range(len(module.instruments)):
    f.write("ins"+str(i)+"D")
    if i == len(module.instruments)-1:
        f.write("\n")
    else:
        f.write(", ")

for i in range(len(module.instruments)):
    features = module.instruments[i].features
    a = filter(
        lambda x: (
            type(x) == InsFeatureMacro
        ), features
    )
    b = filter(
        lambda x: (
            type(x) == InsFeatureFM
        ), features
    )
    arp = [128,0xFF,0xFF]
    #vol = [0xFF,0xFF]
    vol = [0x0F,0xFF,0xFF]

    insChipType = 0
    if module.instruments[i].meta.type == InstrumentType.FDS:
        vol = [0x20,0xFF,0xFF]
        insChipType = 1
    if module.instruments[i].meta.type == InstrumentType.FM_OPLL:
        insChipType = 2

    patch_custom = [0]*8
    duty = [0xFF,0xFF]
    duty2 = []
    if insChipType == 2:
        patch = 0
        for j in b:
            print(j.op_list[0])
            print(j.op_list[1])
            patch = j.opll_preset
            """
            $00 	TVSK MMMM 	Modulator tremolo (T), vibrato (V), sustain (S), key rate scaling (K), multiplier (M)
            $01 	TVSK MMMM 	Carrier tremolo (T), vibrato (V), sustain (S), key rate scaling (K), multiplier (M)
            $02 	KKOO OOOO 	Modulator key level scaling (K), output level (O)
            $03 	KK-Q WFFF 	Carrier key level scaling (K), unused (-), carrier waveform (Q), modulator waveform (W), feedback (F)
            $04 	AAAA DDDD 	Modulator attack (A), decay (D)
            $05 	AAAA DDDD 	Carrier attack (A), decay (D)
            $06 	SSSS RRRR 	Modulator sustain (S), release (R)
            $07 	SSSS RRRR 	Carrier sustain (S), release (R)
            """
            patch_custom[3] = j.fb&7
            patch_custom[3] |= (j.ams)<<3
            patch_custom[3] |= (j.fms)<<4
            patch_custom[2] = j.op_list[0].tl&63
            for k in range(2):
                patch_custom[k] = j.op_list[k].mult&15
                if j.op_list[k].am:
                    patch_custom[k] |= 0x80
                if j.op_list[k].vib:
                    patch_custom[k] |= 0x40
                if j.op_list[k].ssg_env:
                    patch_custom[k] |= 0x20
                if j.op_list[k].ksr:
                    patch_custom[k] |= 0x10
                patch_custom[2+k] |= (j.op_list[k].ksl&3)<<6
                patch_custom[4+k] = ((j.op_list[k].ar&15)<<4)|(j.op_list[k].dr&15)
                patch_custom[6+k] = ((j.op_list[k].sl&15)<<4)|(j.op_list[k].rr&15)
        if patch == 0:
            duty = [0xFE] + patch_custom
        else:
            duty = []
        duty.extend([patch,0xFF,0xFF])
        duty2 = duty
    macros = []
    for j in a:
        macros = j.macros
    hasRelTotal = [0,0,0]
    for j in macros:
        kind = j.kind
        if kind == MacroCode.VOL:
            s = j.speed
            vol = []
            loop = 0xff
            loop2 = 0xff
            hasRel = 0
            for k in j.data:
                if k == MacroItem.LOOP:
                    loop = loop2
                elif k == MacroItem.RELEASE:
                    loop2 = len(vol)
                    vol.append(0xFF)
                    vol.append(loop)
                    relV.append(len(vol))
                    hasRel = 1
                else:
                    loop2 = len(vol)
                    vol.append(k)
            if hasRel == 0:
                relV.append(len(vol)+1)
            hasRelTotal[0] = 1
            vol.append(0xFF)
            vol.append(loop)
        if kind == MacroCode.ARP:
            s = j.speed
            arp = []
            loop = 0xff
            hasRel = 0
            oldlen = 0
            for k in j.data:
                if k == MacroItem.LOOP:
                    loop = oldlen
                elif k == MacroItem.RELEASE:
                    oldlen = len(arp)
                    arp.append(0xFF)
                    arp.append(loop)
                    relA.append(len(arp))
                    hasRel = 1
                elif (k>>30) > 0:
                    oldlen = len(arp)
                    arp.append(0xFE)
                    print(i,k^(1<<30))
                    arp.append(abs(k^(1<<30))%120)
                else:
                    oldlen = len(arp)
                    if k < 0:
                        arp.append((k%120)-120+128)
                    else:
                        arp.append((k%120)+128)
            if hasRel == 0:
                relA.append(len(arp)+1)
            hasRelTotal[1] = 1
            arp.append(0xFF)
            arp.append(loop)
        if kind == MacroCode.DUTY and insChipType == 0:
            s = j.speed
            duty = []
            loop = 0xff
            loop2 = 0xff
            hasRel = 0
            for k in j.data:
                if k == MacroItem.LOOP:
                    loop = loop2
                elif k == MacroItem.RELEASE:
                    duty.append(0xFF)
                    duty.append(loop)
                    relD.append(len(arp))
                    hasRel = 1
                else:
                    loop2 = len(duty)
                    duty.append(k)
            if hasRel == 0:
                relD.append(len(duty)+1)
            hasRelTotal[2] = 1
            duty.append(0xFF)
            duty.append(loop)
        if kind == MacroCode.WAVE and (insChipType == 1 or insChipType == 2):
            s = j.speed
            duty = []
            if insChipType == 2:
                duty = duty2
            loop = 0xff
            loop2 = 0xff
            hasRel = 0
            for k in j.data:
                if k == MacroItem.LOOP:
                    loop = len(duty)
                elif k == MacroItem.RELEASE:
                    loop2 = len(vol)
                    duty.append(0xFF)
                    duty.append(loop)
                    relD.append(len(arp))
                    hasRel = 1
                else:
                    loop2 = len(vol)
                    duty.append(k)
            if hasRel == 0:
                relD.append(len(duty)+1)
            hasRelTotal[2] = 1
            duty.append(0xFF)
            duty.append(loop)
    if hasRelTotal[0] == 0:
        relV.append(0)
    if hasRelTotal[1] == 0:
        relA.append(0)
    if hasRelTotal[2] == 0:
        relD.append(0)
    vol = str(vol)[1:-1]
    duty = str(duty)[1:-1]
    arp = str(arp)[1:-1]
    f.write("ins"+str(i)+"V:\n")
    f.write(".byte "+vol+"\n")
    f.write("ins"+str(i)+"A:\n")
    f.write(".byte "+arp+"\n")
    f.write("ins"+str(i)+"D:\n")
    f.write(".byte "+duty+"\n")


relV = str(relV)[1:-1]
relD = str(relD)[1:-1]
relA = str(relA)[1:-1]
f.write("insVrel:\n")
f.write(".byte "+relV+"\n")
f.write("insArel:\n")
f.write(".byte "+relA+"\n")
f.write("insDrel:\n")
f.write(".byte "+relD+"\n")

for i in range(module.get_num_channels()):
    order = module.subsongs[subsong].order[i]
    f.write("order"+str(i)+"len = "+str(len(order))+"\n")
    f.write("order"+str(i)+"L:\n")
    f.write(".byte ")
    for o in range(len(order)):
        f.write("<(patCH"+str(i)+"N"+str(order[o])+"-1)")
        if o == len(order)-1:
            f.write("\n")
        else:
            f.write(", ")
    f.write("order"+str(i)+"H:\n")
    f.write(".byte ")
    for o in range(len(order)):
        f.write("(>(patCH"+str(i)+"N"+str(order[o])+"-1)&15)")
        if o == len(order)-1:
            f.write("\n")
        else:
            f.write(", ")
    f.write("order"+str(i)+"B:\n")
    f.write(".byte ")
    for o in range(len(order)):
        f.write("(>(patCH"+str(i)+"N"+str(order[o])+"-1)>>4)|")
        f.write("(^(patCH"+str(i)+"N"+str(order[o])+"-1)<<4&240)&255")
        if o == len(order)-1:
            f.write("\n")
        else:
            f.write(", ")

if len(module.wavetables) > 0:
    for i in range(len(module.wavetables)):
        f.write("wavtbl"+str(i)+":\n")
        data = module.wavetables[i].data
        wav = []
        for j in range(64):
            k = int((j/63)*(module.wavetables[i].meta.width-1))
            wav.append(int((data[k]/(module.wavetables[i].meta.height-1))*63))
        f.write(".byte "+str(wav)[1:-1]+"\n")
    f.write("wavL:\n.lobytes ")
    for i in range(len(module.wavetables)):
        f.write("wavtbl"+str(i))
        if i == (len(module.wavetables)-1):
            f.write("\n")
        else:
            f.write(", ")
    f.write("wavH:\n.hibytes ")
    for i in range(len(module.wavetables)):
        f.write("wavtbl"+str(i))
        if i == (len(module.wavetables)-1):
            f.write("\n")
        else:
            f.write(", ")

total_maps = []
for i in range(len(module.instruments)):
    features = module.instruments[i].features
    a = filter(
        lambda x: (
            type(x) == InsFeatureDPCMMap
        ), features
    )
    use_map = False
    for j in a:
        if j.use_map == True:
            use_map = True
            break
    a = filter(
        lambda x: (
            type(x) == InsFeatureDPCMMap
        ), features
    )
    f.write("ins"+str(i)+"DP:\n")
    if use_map == True:
        f.write(".byte ")
        for j in a:
            for k in range(len(j.sample_map)):
                f.write(str(j.sample_map[k].pitch))
                if k == (len(j.sample_map)-1):
                    f.write("\n")
                else:
                    f.write(", ")
            break
    else:
        f.write(".byte 255\n")
    f.write("ins"+str(i)+"DI:\n")
    a = filter(
        lambda x: (
            type(x) == InsFeatureAmiga
        ), features
    )
    if use_map == True:
        f.write(".byte ")
        for j in a:
            for k in range(len(j.sample_map)):
                if j.sample_map[k].sample_index == 65535:
                    f.write("0")
                else:
                    f.write(str(j.sample_map[k].sample_index))
                if k == (len(j.sample_map)-1):
                    f.write("\n")
                else:
                    f.write(", ")
            break
    else:
        init_sample = 0
        for j in a:
            if j.init_sample > 0 and j.init_sample != 65535:
                init_sample = j.init_sample
                break
        f.write(".byte "+str(init_sample)+"\n")

f.write("insDPCMIL:\n")
f.write(".lobytes ")
for i in range(len(module.instruments)):
    f.write("ins"+str(i)+"DI")
    if i == len(module.instruments)-1:
        f.write("\n")
    else:
        f.write(", ")
f.write("insDPCMIH:\n")
f.write(".hibytes ")
for i in range(len(module.instruments)):
    f.write("ins"+str(i)+"DI")
    if i == len(module.instruments)-1:
        f.write("\n")
    else:
        f.write(", ")

f.write("insDPCMPL:\n")
f.write(".lobytes ")
for i in range(len(module.instruments)):
    f.write("ins"+str(i)+"DP")
    if i == len(module.instruments)-1:
        f.write("\n")
    else:
        f.write(", ")
f.write("insDPCMPH:\n")
f.write(".hibytes ")
for i in range(len(module.instruments)):
    f.write("ins"+str(i)+"DP")
    if i == len(module.instruments)-1:
        f.write("\n")
    else:
        f.write(", ")

f.write("sampleA:\n")
if len(module.samples) > 0:
    f.write(".byte ")
    for o in range(len(module.samples)):
        f.write("(DPCM"+str(o)+"&8191)>>6")
        if o == len(module.samples)-1:
            f.write("\n")
        else:
            f.write(", ")

f.write("sampleB:\n")
if len(module.samples) > 0:
    f.write(".byte ")
    for o in range(len(module.samples)):
        f.write("(>(DPCM"+str(o)+")>>5)|")
        f.write("(^(DPCM"+str(o)+")<<3&248)&255")
        if o == len(module.samples)-1:
            f.write("\n")
        else:
            f.write(", ")


f.write("sampleC:\n")
if len(module.samples) > 0:
    f.write(".byte ")
    for o in range(len(module.samples)):
        sample = [int(i) for i in list(module.samples[o].data)]
        if len(sample) == 0:
           sample.extend([0x55]*64)
        while (len(sample)%64) != 0:
            sample.append(0x55)
        f.write(str(max((len(sample)-1)>>4,1)))
        if o == len(module.samples)-1:
            f.write("\n")
        else:
            f.write(", ")

f.write(".segment \"DATA\"\n")
f.write(".org $0000\n")

for i in range(module.get_num_channels()):
    order = module.subsongs[subsong].order[i]
    avail_patterns = filter(
        lambda x: (
            x.channel == i and
            x.subsong == subsong
        ),
        module.patterns
    )
    for p in avail_patterns:
        patnum = p.index
        #print(patnum,i)
        g = str(conv_pattern(p))[1:-1]
        f.write("patCH"+str(i)+"N"+str(patnum)+":\n")
        f.write(".byte "+g+"\n")

f.write(".res 8192-(*&8191), 0\n")

sample_memory = 0
for i in range(len(module.samples)):
    sample = [int(j) for j in list(module.samples[i].data)]
    if len(sample) == 0:
        sample.extend([0x55]*64)
    while (len(sample)%64) != 0:
        sample.append(0x55)
    if 8191-(sample_memory+len(sample)) < 256:
        if len(sample) > 8000:
            raise Exception("DPCM sample is too big to fit in one bank")
        elif 8191-(sample_memory+len(sample)) < 8:
            f.write(".res "+str(8192-sample_memory)+", 0\n")
            f.write("DPCM"+str(i)+":\n.byte "+str(sample)[1:-1]+"\n")
            sample_memory = len(sample)
        else:
            f.write("DPCM"+str(i)+":\n.byte "+str(sample)[1:-1]+"\n")
            sample_memory += len(sample)
            f.write(".res "+str(8192-sample_memory)+", 0\n")
            sample_memory = 0
    else:
        sample_memory += len(sample)
        f.write("DPCM"+str(i)+":\n.byte "+str(sample)[1:-1]+"\n")
f.close()
