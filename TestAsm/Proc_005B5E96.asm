; D:\Delphi Projects\Deobfuscator\TestExe\Test_ver 2.3.7\Test_32_Dolphin.exe

;<005B5E96>

Bits 32

push ax
pop ax

neg al
neg ax
neg cl
neg ecx

mov al,cl

mov al, 0x56


add ah,0x37

inc dword [ebp+ebx]
neg dword [ebp+ebx]

inc byte [ebx]
neg byte [ebx+ebp]

xchg eax,ecx
xchg dword [eax],edx

mov dword [ebp+ebx],eax
mov dword [ebp+ebx],0x401000

mov dword [ebp+ebx*4+0x401000],0x401000


8E84FF: push esi
8E8500: mov dword ds:[esp],0x7fde6f71
8E8507: neg dword ds:[esp]
8E850A: add dword ds:[esp],0xff
8E850E: not dword ds:[esp]
8E8511: shl dword ds:[esp],04
8E8515: push esi
8E8516: mov dword ds:[esp],ecx
8E8519: mov ecx,0x0219099c
8E851E: add dword ds:[esp+0x04],ecx
8E8522: pop ecx
8E8523: push edx
8E8524: mov dword ds:[esp],0x7dffd441
8E852B: xor dword ds:[esp],0x3ffb7b92
8E8532: push ebx
8E8533: mov ebx,0x7f7f834f
8E8538: add dword ds:[esp+0x04],0x59a4123a
8E8540: add dword ds:[esp+0x04],ebx
8E8544: push edi
8E8545: mov edi,0x59a4123a
8E854A: sub dword ds:[esp+0x08],edi
8E854E: pop edi
8E854F: pop ebx
8E8550: sub dword ds:[esp],0xc135cdc4
8E8557: push edx
8E8558: mov dword ds:[esp],0x7bffe277
8E855F: add dword ds:[esp],0x6e8feefe
8E8566: push esi
8E8567: mov esi,0x7efe9e93
8E856C: and dword ds:[esp+0x04],esi
8E8570: mov esi,dword ds:[esp]
8E8573: add esp,00000004
8E8579: shl dword ds:[esp],03
8E857D: xor dword ds:[esp],0x61fedb88
8E8584: mov dword ds:[esp],eax
8E8587: push edi
8E8588: push esp
8E8589: pop edi
8E858A: add edi,00000004
8E8590: sub edi,04
8E8593: xchg dword ds:[esp],edi
8E8596: mov esp,dword ds:[esp]
8E8599: mov dword ds:[esp],edi
8E859C: mov dword ds:[esp],ebx
8E859F: push dword ds:[esp+0x10]
8E85A3: mov eax,dword ds:[esp]
8E85A6: sub esp,00000004
8E85AC: mov dword ds:[esp],ebp
8E85AF: push esp
8E85B0: pop ebp
8E85B1: add ebp,00000004
8E85B7: add ebp,04
8E85BA: xchg dword ds:[esp],ebp
8E85BD: pop esp
8E85BE: push dword ds:[esp+0x08]
8E85C2: mov ebx,dword ds:[esp]
8E85C5: push edi
8E85C6: push esp
8E85C7: mov edi,dword ds:[esp]
8E85CA: add esp,00000004
8E85D0: add edi,00000004
8E85D6: add edi,00000004
8E85DC: xchg dword ds:[esp],edi
8E85DF: pop esp
8E85E0: mov dword ds:[esp+0x08],eax
8E85E4: mov dword ds:[esp+0x10],ebx
8E85E8: push dword ds:[esp]
8E85EB: pop ebx
8E85EC: push 0x2ea8d8ff
8E85F1: mov dword ds:[esp],ebx
8E85F4: mov ebx,esp
8E85F6: push ecx
8E85F7: mov ecx,00000004
8E85FC: add ebx,ecx
8E85FE: pop ecx
8E85FF: add ebx,00000004
8E8605: xchg dword ds:[esp],ebx
8E8608: pop esp
8E8609: push dword ds:[esp]
8E860C: push dword ds:[esp]
8E860F: pop eax
8E8610: add esp,04
8E8613: push ecx
8E8614: mov ecx,esp
8E8616: add ecx,00000004
8E861C: add ecx,04
8E861F: xchg dword ds:[esp],ecx
8E8622: pop esp
824228: push esi
824229: push eax
82422A: push edx
82422B: pushad
82422C: popad
82422F: push eax
824230: push edx
824233: pop edx
824234: pop eax
824235: pop edx
824236: pop eax
82424D: pop esi
82425B: sub esp,04
82425E: mov dword ds:[esp],ecx
824261: mov dword ds:[esp],0x34f6695d
824268: not dword ds:[esp]
82426B: xor dword ds:[esp],0x29be7485
824272: or dword ds:[esp],0x4e4f82b4
824279: push edx
82427A: mov edx,0x33c4ad4c
82427F: add dword ds:[esp+0x04],edx
824283: pop edx
824284: mov dword ds:[esp],edx
824287: mov dword ds:[esp],0x1d968181
82428E: add dword ds:[esp],0x34274be4
824295: xchg dword ds:[esp],edi
824298: push edi
824299: not dword ds:[esp]
82429C: mov edi,dword ds:[esp]
82429F: add esp,00000004
8242A5: push edi
8242A6: push dword ds:[esp+0x04]
8242AA: mov edi,dword ds:[esp]
8242AD: add esp,00000004
8242B3: pop dword ds:[esp]
8242B6: push eax
8242B7: mov eax,0x7f7e6e00
8242BC: and dword ds:[esp+0x04],eax
8242C0: pop eax
8242C1: push eax
8242C2: mov eax,0xfcacfbc0
8242C7: add dword ds:[esp+0x04],eax
8242CB: pop eax
8242CC: add dword ds:[esp],01
8242D0: push edx
8242D1: mov edx,0x49501004
8242D6: add dword ds:[esp+0x04],0x4aff5bb7
8242DE: add dword ds:[esp+0x04],edx
8242E2: sub dword ds:[esp+0x04],0x4aff5bb7
8242EA: pop edx
8242EB: mov dword ds:[esp],eax
8242EE: sub esp,00000004
8242F4: mov dword ds:[esp],ebp
8242F7: mov ebp,esp
8242F9: push 0x61d220a2
8242FE: mov dword ds:[esp],edi
824301: mov edi,0x5fef8422
824306: push ebp
824307: mov ebp,0x5fef8426
82430C: xor edi,ebp
82430E: pop ebp
82430F: sub ebp,0x7770055b
824315: add ebp,edi
824317: push ecx
824318: sub esp,00000004
82431E: mov dword ds:[esp],edx
824321: mov edx,0x7fdb61af
824326: mov ecx,0x38645f4b
82432B: xor ecx,edx
82432D: pop edx
82432E: push ebp
82432F: push ebx
824330: mov ebx,0x7fc4072d
824335: push 0xbb0314de
82433A: pop ebp
82433B: sub ebp,ebx
82433D: pop ebx
82433E: push esi
82433F: mov esi,00000001
824344: add ebp,esi
824346: pop esi
824347: add ebp,0x20b912dc
82434D: and ecx,ebp
82434F: pop ebp
824350: add ecx,0x7bb57bc4
824356: sub ecx,0x47fd96ed
82435C: add ebp,ecx
82435E: pop ecx
82435F: mov edi,dword ds:[esp]
824362: add esp,04
824365: sub ebp,00000004
82436B: sub esp,04
82436E: mov dword ds:[esp],ebx
824371: mov dword ds:[esp],0x35472290
824378: mov dword ds:[esp],ebp
82437B: push dword ds:[esp+0x04]
82437F: pop ebp
824380: pop dword ds:[esp]
824383: pop esp
824384: mov dword ds:[esp],ecx
824387: push 0x4ee42e0a
82438C: mov dword ds:[esp],esi
82438F: mov esi,esp
824391: push ebx
824392: mov ebx,esp
824394: add ebx,00000004
82439A: sub ebx,04
82439D: xchg dword ds:[esp],ebx
8243A0: pop esp
8243A1: mov dword ds:[esp],eax
8243A4: push ecx
8243A5: mov ecx,00000004
8243AA: push ecx
8243AB: add dword ds:[esp],0x6cf522e7
8243B2: pop eax
8243B3: sub eax,0x6cf522e7
8243B8: mov ecx,dword ds:[esp]
8243BB: add esp,04
8243BE: push eax
8243BF: mov eax,0x69fed6eb
8243C4: and eax,0x5b5e9172
8243C9: push esi
8243CA: mov dword ds:[esp],ecx
8243CD: mov ecx,0x7e179770
8243D2: push edi
8243D3: mov edi,0x39ff34f5
8243D8: add edi,0x7f3facfd
8243DE: or edi,0x7d791193
8243E4: dec edi
8243E5: sub edi,0x7e5fea25
8243EB: dec edi
8243EC: add edi,0x90fcab0a
8243F2: add ecx,0x26759c33
8243F8: sub ecx,edi
8243FA: sub ecx,0x26759c33
824400: pop edi
824401: or eax,ecx
824403: pop ecx
824404: sub eax,0xedff8200
824409: sub esi,eax
82440B: pop eax
82440C: add esi,eax
82440E: add esi,0x7fff72fa
824414: mov eax,dword ds:[esp]
824417: add esp,04
82441A: push ebp
82441B: mov dword ds:[esp],edx
82441E: mov edx,00000004
824423: sub esi,edx
824425: pop edx
824426: xchg dword ds:[esp],esi
824429: mov esp,dword ds:[esp]
82442C: mov dword ds:[esp],ecx
82442F: mov dword ds:[esp],edx
824432: push eax
824433: mov dword ds:[esp],esi
824436: mov esi,esp
824438: push ebp
824439: mov ebp,0x4d7f2efa
82443E: dec ebp
82443F: or ebp,0x7fcfb901
824445: add ebp,0x8000400b
82444B: add esi,ebp
82444D: pop ebp
82444E: sub esi,04
824451: xchg dword ds:[esp],esi
824454: pop esp
824455: mov dword ds:[esp],ecx
824458: push 0x38a4792a
82445D: push eax
82445E: mov eax,esp
824460: add eax,00000004
824465: sub eax,00000004
82446A: xchg dword ds:[esp],eax
82446D: pop esp
82446E: mov dword ds:[esp],esp
824471: push ebx
824472: mov ebx,00000004
824477: add dword ds:[esp+0x04],ebx
82447B: pop ebx
82447C: pop dword ds:[esp]
82447F: add dword ds:[esp],00000004
824486: push dword ds:[esp]
824489: pop ecx
82448A: add esp,04
82448D: push edx
82448E: push ebp
82448F: mov ebp,0x5f415f1d
824494: mov edx,0xa0bea0e7
824499: add edx,ebp
82449B: mov ebp,dword ds:[esp]
82449E: add esp,04
8244A1: add ecx,edx
8244A3: pop edx
8244A4: sub ecx,04
8244A7: push ebp
8244A8: mov dword ds:[esp],ecx
8244AB: push dword ds:[esp+0x04]
8244AF: pop ecx
8244B0: pop dword ds:[esp]
8244B3: mov esp,dword ds:[esp]
8244B6: mov dword ds:[esp],ebx
8244B9: sub esp,00000004
8244BF: mov dword ds:[esp],ebp
8244C2: push esp
8244C3: push dword ds:[esp]
8244C6: pop ebp
8244C7: push ebx
8244C8: mov ebx,esp
8244CA: add ebx,00000004
8244D0: push eax
8244D1: mov eax,00000004
8244D6: add ebx,eax
8244D8: mov eax,dword ds:[esp]
8244DB: add esp,04
8244DE: xchg dword ds:[esp],ebx
8244E1: pop esp
8244E2: push edi
8244E3: mov edi,0x7b5f0fbc
8244E8: inc edi
8244E9: add edi,0x6dff72db
8244EF: or edi,0x7effef1e
8244F5: inc edi
8244F6: shr edi,08
8244F9: add edi,0xff000015
8244FF: add ebp,0x0daa3bb0
824505: add ebp,edi
824507: sub esp,00000004
82450D: mov dword ds:[esp],edx
824510: mov edx,0x6ff74aa1
824515: shl edx,01
824518: add edx,0x2dbba66e
82451E: sub ebp,edx
824520: pop edx
824521: push dword ds:[esp]
824524: pop edi
824525: add esp,00000004
82452B: push ecx
82452C: mov ecx,00000004
824531: sub ebp,ecx
824533: mov ecx,dword ds:[esp]
824536: add esp,00000004
82453C: xor ebp,dword ds:[esp]
82453F: xor dword ds:[esp],ebp
824542: xor ebp,dword ds:[esp]
824545: mov esp,dword ds:[esp]
824548: mov dword ds:[esp],ebx
82454B: push 0x604d6ab2
824550: mov dword ds:[esp],edx
824553: mov edx,esp
824555: add edx,00000004
82455B: push edi
82455C: push ebx
82455D: mov ebx,0x3ed16c75
824562: dec ebx
824563: push edx
824564: mov edx,0x69bfe1b2
824569: and edx,0x66fef7fa
82456F: inc edx
824570: shr edx,07
824573: shl edx,05
824576: dec edx
824577: push esi
824578: mov esi,0x77b4844b
82457D: neg esi
82457F: add esi,0x57efd55f
824585: shr esi,02
824588: add esi,0xce4304ac
82458E: sub edx,esi
824590: pop esi
824591: sub ebx,edx
824593: pop edx
824594: push ebx
824595: sub dword ds:[esp],0x5fffd1a4
82459C: mov edi,dword ds:[esp]
82459F: add esp,04
8245A2: add edi,0x5fffd1a4
8245A8: pop ebx
8245A9: shl edi,01
8245AC: shl edi,04
8245AF: shr edi,04
8245B2: shr edi,02
8245B5: sub esp,04
8245B8: mov dword ds:[esp],esi
8245BB: mov esi,00000001
8245C0: sub edi,0x3fd7e856
8245C6: push esi
8245C7: mov esi,0x7c7b09b1
8245CC: add edi,esi
8245CE: pop esi
8245CF: add edi,0x7bfbde00
8245D5: add edi,esi
8245D7: sub edi,0x7bfbde00
8245DD: sub edi,0x7c7b09b1
8245E3: add edi,0x3fd7e856
8245E9: pop esi
8245EA: xor edi,0x0279c680
8245F0: add edx,0x7b3316e6
8245F6: add edx,0x4fa69e6d
8245FC: sub edx,0x5bdf43c9
824602: sub edx,edi
824604: add edx,0x5bdf43c9
82460A: sub edx,0x4fa69e6d
824610: sub edx,0x7b3316e6
824616: pop edi
824617: xor edx,dword ds:[esp]
82461A: xor dword ds:[esp],edx
82461D: xor edx,dword ds:[esp]
824620: pop esp
824621: mov dword ds:[esp],ebp
824624: sub esp,04
824627: mov dword ds:[esp],edi
82462A: mov dword ds:[esp],eax
82462D: push esi
82462E: mov dword ds:[esp],ebx
824631: push ecx
824632: mov dword ds:[esp],0x5f7de920
824639: sub dword ds:[esp],0x7e9f64b3
824640: sub dword ds:[esp],0x5f3f1c91
824647: add dword ds:[esp],0xf99cc708
82464E: push dword ds:[esp]
824651: pop ebx
824652: add esp,00000004
824658: mov dword ds:[esp+0x04],ebx
82465C: push dword ds:[esp]
82465F: mov ebx,dword ds:[esp]
824662: add esp,04
824665: push edi
824666: mov edi,esp
824668: add edi,00000004
82466E: push edx
82466F: mov edx,00000004
824674: add edi,0x6ffe5aad
82467A: sub edi,0x7ecb8307
824680: add edi,edx
824682: add edi,0x7ecb8307
824688: sub edi,0x6ffe5aad
82468E: pop edx
82468F: xor edi,dword ds:[esp]
824692: xor dword ds:[esp],edi
824695: xor edi,dword ds:[esp]
824698: mov esp,dword ds:[esp]
82469B: mov dword ds:[esp],esi
82469E: sub esp,04
8246A1: mov dword ds:[esp],ecx
8246A4: mov ecx,esp
8246A6: push eax
8246A7: mov eax,00000004
8246AC: push edi
8246AD: mov edi,0x3b7aeee4
8246B2: and edi,0x6fade4f9
8246B8: add edi,0x34d451de
8246BE: sub ecx,0x1f3a7015
8246C4: sub ecx,edi
8246C6: add ecx,0x1f3a7015
8246CC: push dword ds:[esp]
8246CF: pop edi
8246D0: add esp,00000004
8246D6: sub ecx,0x4ef60246
8246DC: add ecx,eax
8246DE: add ecx,0x4ef60246
8246E4: push esi
8246E5: mov esi,0x5ffd36be
8246EA: add ecx,esi
8246EC: push dword ds:[esp]
8246EF: pop esi
8246F0: add esp,04
8246F3: push dword ds:[esp]
8246F6: mov eax,dword ds:[esp]
8246F9: add esp,04
8246FC: push esi
8246FD: mov esi,esp
8246FF: add esi,00000004
824705: add esi,00000004
82470B: xchg dword ds:[esp],esi
82470E: pop esp
82470F: sub ecx,04
824712: push ecx
824713: push dword ds:[esp+0x04]
824717: mov ecx,dword ds:[esp]
82471A: add esp,00000004
824720: pop dword ds:[esp]
824723: mov esp,dword ds:[esp]
824726: mov dword ds:[esp],edi
824729: call  0x0082472e
82472E: push dword ds:[esp]
824731: mov ecx,dword ds:[esp]
824734: push ebp
824735: sub esp,00000004
82473B: mov dword ds:[esp],esp
82473E: add dword ds:[esp],04
824742: pop ebp
824743: add ebp,00000004
824749: push eax
82474A: push 0x33327a8c
82474F: mov dword ds:[esp],esi
824752: mov esi,0x7ff698e4
824757: sub esi,0x63e9aed1
82475D: sub esi,0x3ebfa30f
824763: shr esi,03
824766: xor esi,0x1ba9a8e4
82476C: mov eax,esi
82476E: pop esi
82476F: add ebp,eax
824771: push dword ds:[esp]
824774: pop eax
824775: push edx
824776: mov edx,esp
824778: add edx,00000004
82477E: add edx,00000004
824784: xchg dword ds:[esp],edx
824787: pop esp
824788: push ebp
824789: push dword ds:[esp+0x04]
82478D: mov ebp,dword ds:[esp]
824790: push edx
824791: mov edx,esp
824793: push ecx
824794: mov ecx,0x525aed0a
824799: or ecx,0x73efbc94
82479F: add ecx,0x8c000266
8247A5: add edx,ecx
8247A7: pop ecx
8247A8: push ebx
8247A9: mov ebx,00000004
8247AE: add edx,ebx
8247B0: pop ebx
8247B1: xchg dword ds:[esp],edx
8247B4: pop esp
8247B5: pop dword ds:[esp]
8247B8: mov esp,dword ds:[esp]
8247BB: push 0x78185bc4
8247C0: mov dword ds:[esp],eax
8247C3: mov eax,esp
8247C5: add eax,00000004
8247CA: add eax,04
8247CD: xchg dword ds:[esp],eax
8247D0: pop esp
8247D1: push edi
8247D2: mov edi,0x7bed3d16
8247D7: sub edi,01
8247DA: push esi
8247DB: mov esi,0x7eff75c6
8247E0: xor esi,0x110197b5
8247E6: push eax
8247E7: push edi
8247E8: mov dword ds:[esp],ebx
8247EB: push ecx
8247EC: mov ecx,0x7fdf57c9
8247F1: sub ecx,0x10e4cf5f
8247F7: mov ebx,ecx
8247F9: pop ecx
8247FA: mov eax,ebx
8247FC: pop ebx
8247FD: shl eax,08
824800: and eax,0x7ed53573
824805: push edx
824806: mov edx,0x7dd2fd30
82480B: xor edx,0x600509e3
824811: xor eax,edx
824813: mov edx,dword ds:[esp]
824816: add esp,00000004
82481C: xor eax,0x5ea7cd55
824821: sub eax,0xdac87eda
824826: xor esi,eax
824828: pop eax
824829: push edi
82482A: mov edi,0x07e77743
82482F: or esi,edi
824831: pop edi
824832: inc esi
824833: shl esi,01
824836: xchg esi,ecx
824838: xchg ecx,edx
82483A: not edx
82483C: xchg ecx,edx
82483E: xchg esi,ecx
824840: xor esi,0x9f5eee25
824846: xor edi,esi
824848: pop esi
824849: shr edi,01
82484C: push eax
82484D: mov eax,0x3a596c81
824852: xor edi,eax
824854: pop eax
824855: sub ecx,edi
824857: mov edi,dword ds:[esp]
82485A: push eax
82485B: mov eax,esp
82485D: push ebx
82485E: mov ebx,00000004
824863: add eax,ebx
824865: pop ebx
824866: add eax,00000004
82486B: xor eax,dword ds:[esp]
82486E: xor dword ds:[esp],eax
824871: xor eax,dword ds:[esp]
824874: pop esp
824875: push ebp
824876: mov dword ds:[esp],esi
824879: mov dword ds:[esp],eax
82487C: sub esp,04
82487F: mov dword ds:[esp],esi
824882: mov esi,0x4e5f7000
824887: neg esi
824889: shr esi,01
82488C: add esi,0xa771fa28
824892: push esi
824893: pop eax
824894: push dword ds:[esp]
824897: pop esi
824898: sub esp,04
82489B: mov dword ds:[esp],ecx
82489E: mov ecx,esp
8248A0: push eax
8248A1: mov eax,0x75fb2695
8248A6: shl eax,07
8248A9: and eax,0x7fd6ce96
8248AE: shr eax,05
8248B1: sub eax,0x59dfd121
8248B6: add eax,0x55f33ed1
8248BB: add ecx,eax
8248BD: mov eax,dword ds:[esp]
8248C0: add esp,04
8248C3: push edi
8248C4: mov edi,0x5bf73442
8248C9: add edi,0x1f9f1f3c
8248CF: shl edi,08
8248D2: and edi,0x57f3c3b2
8248D8: push eax
8248D9: mov eax,0x7de7ceb9
8248DE: sub eax,0x7f9fcea5
8248E3: shl eax,07
8248E6: and eax,0x7ffd5294
8248EB: sub eax,0xa70288cb
8248F0: or edi,eax
8248F2: pop eax
8248F3: inc edi
8248F4: sub edi,0x7eff7b32
8248FA: add ecx,edi
8248FC: pop edi
8248FD: xchg dword ds:[esp],ecx
824900: mov esp,dword ds:[esp]
824903: push edi
824904: mov edi,0x763559d7
824909: add ecx,edi
82490B: mov edi,dword ds:[esp]
82490E: add esp,04
824911: sub ecx,eax
824913: sub ecx,0x763559d7
824919: mov eax,dword ds:[esp]
82491C: add esp,04
82491F: push esi
824920: mov dword ds:[esp],edi
824923: mov dword ds:[esp],0x0025685a
82492A: push dword ds:[esp]
82492D: mov ebp,dword ds:[esp]
824930: push eax
824931: mov eax,esp
824933: add eax,00000004
824938: add eax,00000004
82493D: xchg dword ds:[esp],eax
824940: mov esp,dword ds:[esp]
824943: push ebx
824944: push esp
824945: pop ebx
824946: push eax
824947: mov dword ds:[esp],edx
82494A: mov dword ds:[esp],0x22053b90
824951: mov dword ds:[esp],edx
824954: mov edx,0x7ded3fda
824959: neg edx
82495B: neg edx
82495D: push esi
82495E: mov esi,0x7eff462d
824963: neg esi
824965: add esi,0x7ffb08c5
82496B: push edi
82496C: mov edi,0x47df7096
824971: shl edi,07
824974: add edi,0x7fc696ce
82497A: sub esi,edi
82497C: pop edi
82497D: add esi,0xeb80c062
824983: or edx,esi
824985: pop esi
824986: or edx,0x6865052b
82498C: xor edx,0x7dfdbffb
824992: add ebx,edx
824994: mov edx,dword ds:[esp]
824997: add esp,00000004
82499D: push esi
82499E: mov esi,0x7f3e64f2
8249A3: or esi,0x7e77c000
8249A9: xor esi,0x7f7fe4f6
8249AF: sub ebx,0x3fbf052b
8249B5: add ebx,esi
8249B7: add ebx,0x3fbf052b
8249BD: mov esi,dword ds:[esp]
8249C0: add esp,00000004
8249C6: xchg dword ds:[esp],ebx
8249C9: pop esp
8249CA: push 0x65c196ca
8249CF: mov dword ds:[esp],ebp
8249D2: mov dword ds:[esp],eax
8249D5: push ebp
8249D6: mov ebp,0x77dd9c60
8249DB: push ebp
8249DC: pop eax
8249DD: pop ebp
8249DE: push 0x08c849a1
8249E3: mov dword ds:[esp],ecx
8249E6: mov ecx,0x773bf786
8249EB: and eax,ecx
8249ED: pop ecx
8249EE: push edi
8249EF: mov edi,0x7bef2b00
8249F4: sub eax,0x3fff5bd3
8249F9: add eax,edi
8249FB: add eax,0x3fff5bd3
824A00: pop edi
824A01: add eax,0x5f72cf28
824A06: sub ebp,0x2ff50fb0
824A0C: add ebp,eax
824A0E: push ecx
824A0F: mov ecx,0x2be58b90
824A14: push edi
824A15: mov edi,0x5cfa5bc4
824A1A: not edi
824A1C: shr edi,03
824A1F: sub edi,0x79b5bdbf
824A25: dec edi
824A26: sub edi,0x969a72a7
824A2C: xor ecx,edi
824A2E: pop edi
824A2F: add ebp,ecx
824A31: pop ecx
824A32: pop eax
824A33: push edi
824A34: mov edi,0x1defb974
824A39: sub edi,0x7effa3ca
824A3F: or edi,0x12fee8b9
824A45: inc edi
824A46: not edi
824A48: add edi,0x369d906b
824A4E: sub edi,0x5fa6c0f7
824A54: add ebp,edi
824A56: pop edi
824A57: add ebp,0x6bea80f0
824A5D: add ebp,ecx
824A5F: sub ebp,0x6bea80f0
824A65: sub ebp,0x37f7d1b7
824A6B: sub esp,00000004
824A71: mov dword ds:[esp],ecx
824A74: mov dword ds:[esp],edx
824A77: mov edx,0x527b8e28
824A7C: add ebp,0x7fffca29
824A82: sub ebp,edx
824A84: sub ebp,0x7fffca29
824A8A: pop edx
824A8B: push eax
824A8C: mov dword ds:[esp],0x7f69991e
824A93: push esi
824A94: mov esi,0xffffffff
824A99: add dword ds:[esp+0x04],esi
824A9D: push dword ds:[esp]
824AA0: pop esi
824AA1: push eax
824AA2: mov eax,esp
824AA4: sub esp,00000004
824AAA: mov dword ds:[esp],edi
824AAD: mov edi,00000004
824AB2: add eax,edi
824AB4: pop edi
824AB5: add eax,00000004
824ABA: xchg dword ds:[esp],eax
824ABD: mov esp,dword ds:[esp]
824AC0: not dword ds:[esp]
824AC3: push ecx
824AC4: mov ecx,0x79e8f176
824AC9: add ecx,0x05ea770d
824ACF: xor dword ds:[esp+0x04],ecx
824AD3: push dword ds:[esp]
824AD6: pop ecx
824AD7: add esp,04
824ADA: xchg dword ds:[esp],esi
824ADD: not esi
824ADF: xchg dword ds:[esp],esi
824AE2: push eax
824AE3: mov eax,0x78efe500
824AE8: add dword ds:[esp+0x04],eax
824AEC: mov eax,dword ds:[esp]
824AEF: add esp,00000004
824AF5: push eax
824AF6: mov eax,0x9fdc9f06
824AFB: add dword ds:[esp+0x04],eax
824AFF: pop eax
824B00: mov dword ds:[esp],ecx
824B03: push edi
824B04: push eax
824B05: mov dword ds:[esp],esp
824B08: add dword ds:[esp],00000004
824B0F: push dword ds:[esp]
824B12: pop edi
824B13: add esp,00000004
824B19: push ecx
824B1A: mov ecx,0x6fc7daa1
824B1F: sub ecx,0x6fc7da9d
824B25: add edi,ecx
824B27: pop ecx
824B28: sub edi,00000004
824B2E: push edi
824B2F: push dword ds:[esp+0x04]
824B33: pop edi
824B34: pop dword ds:[esp]
824B37: mov esp,dword ds:[esp]
824B3A: mov dword ds:[esp],edx
824B3D: mov dword ds:[esp],0x7ff7702f
824B44: not dword ds:[esp]
824B47: and dword ds:[esp],0x6fcf5540
824B4E: sub esp,04
824B51: mov dword ds:[esp],eax
824B54: mov eax,0x3eefd2dc
824B59: xor dword ds:[esp+0x04],eax
824B5D: pop eax
824B5E: dec dword ds:[esp]
824B61: xor dword ds:[esp],0x3ee7d79a
824B68: mov ecx,dword ds:[esp]
824B6B: push edx
824B6C: mov edx,esp
824B6E: add edx,00000004
824B74: push ebp
824B75: mov ebp,00000004
824B7A: add edx,0x4f551ef6
824B80: add edx,0x36fe5a0f
824B86: add edx,ebp
824B88: sub edx,0x36fe5a0f
824B8E: sub edx,0x4f551ef6
824B94: mov ebp,dword ds:[esp]
824B97: add esp,00000004
824B9D: xchg dword ds:[esp],edx
824BA0: pop esp
824BA1: push 0x7cfee0f1
824BA6: pop ebx
824BA7: push ebp
824BA8: push 0x0eb0116e
824BAD: mov dword ds:[esp],edx
824BB0: push ebx
824BB1: mov dword ds:[esp],0x74c5b56b
824BB8: mov edx,dword ds:[esp]
824BBB: add esp,04
824BBE: push ecx
824BBF: mov ecx,edx
824BC1: mov ebp,ecx
824BC3: pop ecx
824BC4: pop edx
824BC5: neg ebp
824BC7: and ebp,0x2f7f5d25
824BCD: dec ebp
824BCE: shr ebp,02
824BD1: xor ebp,0x4d39c38a
824BD7: and ebx,ebp
824BD9: pop ebp
824BDA: xor ebx,0x73ade192
824BE0: shl ebx,06
824BE3: shl ebx,03
824BE6: push ecx
824BE7: push edi
824BE8: mov edi,0x6fbd57b0
824BED: xor edi,0x9042a84f
824BF3: push edi
824BF4: pop ecx
824BF5: pop edi
824BF6: sub ebx,ecx
824BF8: pop ecx
824BF9: push edi
824BFA: mov edi,0x76dfdd00
824BFF: not edi
824C01: xor edi,0x72deac9f
824C07: push ebp
824C08: mov ebp,0x34fe6627
824C0D: neg ebp
824C0F: push 00000000
824C14: sub dword ds:[esp],ebp
824C17: pop ebp
824C18: sub ebp,0xf041fde8
824C1E: push edx
824C1F: mov edx,0x567f12c1
824C24: add edi,0x637ac720
824C2A: sub edi,edx
824C2C: sub edi,0x637ac720
824C32: pop edx
824C33: sub edi,ebp
824C35: add edi,0x567f12c1
824C3B: mov ebp,dword ds:[esp]
824C3E: add esp,04
824C41: xor ebx,edi
824C43: pop edi
824C44: xor eax,eax
824C46: lock cmpxchg dword ds:[ebp+ebx+0x00],ecx
824C4C: jz  0x00824c59
824C52: pause
824C54: jmp  0x00824c44
824C59: mov ecx,dword ds:[esp]
824C5C: push edx
824C5D: mov dword ds:[esp],ebp
824C60: mov ebp,esp
824C62: push ebx
824C63: mov ebx,00000004
824C68: add ebp,0x37bbe761
824C6E: add ebp,ebx
824C70: sub ebp,0x37bbe761
824C76: pop ebx
824C77: push edx
824C78: push edi
824C79: mov dword ds:[esp],ecx
824C7C: mov ecx,esp
824C7E: add ecx,00000004
824C84: sub ecx,00000004
824C8A: xchg dword ds:[esp],ecx
824C8D: pop esp
824C8E: mov dword ds:[esp],ebx
824C91: push ebp
824C92: mov ebp,00000004
824C97: mov ebx,ebp
824C99: pop ebp
824C9A: mov edx,ebx
824C9C: mov ebx,dword ds:[esp]
824C9F: add esp,04
824CA2: add ebp,0x6ffd1f1a
824CA8: add ebp,0x3f7ff792
824CAE: push eax
824CAF: mov eax,0x7eff1417
824CB4: add ebp,0x39f6be76
824CBA: add ebp,0x7e6bad78
824CC0: add ebp,eax
824CC2: sub ebp,0x7e6bad78
824CC8: push eax
824CC9: mov eax,0x23de2f59
824CCE: and eax,0x72bf9f58
824CD3: add eax,0x1758af1e
824CD8: sub ebp,eax
824CDA: pop eax
824CDB: pop eax
824CDC: add ebp,edx
824CDE: sub ebp,0x7eff1417
824CE4: sub ebp,0x3f7ff792
824CEA: sub ebp,0x6ffd1f1a
824CF0: pop edx
824CF1: xchg dword ds:[esp],ebp
824CF4: mov esp,dword ds:[esp]
824CF7: push esi
824CF8: push edi
824CF9: push 0x6b3ef85d
824CFE: pop edi
824CFF: xor edi,0x5019123a
824D05: push ecx
824D06: mov ecx,edi
824D08: mov esi,ecx
824D0A: pop ecx
824D0B: pop edi
824D0C: push esi
824D0D: not dword ds:[esp]
824D10: pop esi
824D11: push esi
824D12: not dword ds:[esp]
824D15: pop esi
824D16: shr esi,07
824D19: not esi
824D1B: push ecx
824D1C: mov dword ds:[esp],eax
824D1F: mov eax,0xff89af94
824D24: sub esi,0x7fa705a9
824D2A: sub esi,eax
824D2C: push ebp
824D2D: mov ebp,0x7fa705a9
824D32: add esi,ebp
824D34: pop ebp
824D35: push dword ds:[esp]
824D38: mov eax,dword ds:[esp]
824D3B: add esp,04
824D3E: push eax
824D3F: mov dword ds:[esp],esi
824D42: mov esi,esp
824D44: add esi,00000004
824D4A: add esi,04
824D4D: xchg dword ds:[esp],esi
824D50: pop esp
824D51: push 0x052cbddc
824D56: mov dword ds:[esp],esi
824D59: push dword ds:[esp]
824D5C: pop ebx
824D5D: push eax
824D5E: mov eax,esp
824D60: add eax,00000004
824D65: add eax,04
824D68: xchg dword ds:[esp],eax
824D6B: pop esp
824D6C: mov esi,dword ds:[esp]
824D6F: add esp,00000004
824D75: push ebx
824D76: mov dword ds:[esp],edx
824D79: sub esp,00000004
824D7F: mov dword ds:[esp],eax
824D82: mov dword ds:[esp],ecx
824D85: mov edx,dword ds:[esp]
824D88: add esp,00000004
824D8E: push ecx
824D8F: push ebx
824D90: sub dword ds:[esp],0x5fbe7da0
824D97: pop ecx
824D98: add ecx,0x5fbe7da0
824D9E: add ecx,00000000
824DA4: sub ecx,0x5e7a7087
824DAA: add ecx,ebp
824DAC: add ecx,0x5e7a7087
824DB2: push edx
824DB3: sub dword ds:[esp],0x7ffa3dce
824DBA: pop dword ds:[ecx]
824DBC: add dword ds:[ecx],0x7ffa3dce
824DC2: mov ecx,dword ds:[esp]
824DC5: add esp,04
824DC8: mov edx,dword ds:[esp]
824DCB: push edi
824DCC: push esp
824DCD: mov edi,dword ds:[esp]
824DD0: add esp,00000004
824DD6: add edi,00000004
824DDC: push esi
824DDD: push 0x3be6e422
824DE2: pop esi
824DE3: not esi
824DE5: shr esi,02
824DE8: xor esi,0x51e77768
824DEE: shr esi,05
824DF1: not esi
824DF3: xor esi,0xfcf8f677
824DF9: add edi,esi
824DFB: pop esi
824DFC: sub esp,04
824DFF: mov dword ds:[esp],eax
824E02: mov dword ds:[esp],edi
824E05: push dword ds:[esp+0x04]
824E09: pop edi
824E0A: pop dword ds:[esp]
824E0D: pop esp
824E0E: sub esp,00000004
824E14: mov dword ds:[esp],edx
824E17: mov dword ds:[esp],eax
824E1A: mov dword ds:[esp],0x6d6f5273
824E21: push esi
824E22: mov esi,0x3ef982a0
824E27: or dword ds:[esp+0x04],esi
824E2B: mov esi,dword ds:[esp]
824E2E: add esp,00000004
824E34: shr dword ds:[esp],05
824E38: push 0x292ca2ea
824E3D: mov dword ds:[esp],edx
824E40: mov dword ds:[esp],ebp
824E43: push esi
824E44: mov esi,0x797b3d32
824E49: dec esi
824E4A: dec esi
824E4B: push ebp
824E4C: mov ebp,0x75bbb359
824E51: sub esi,ebp
824E53: pop ebp
824E54: add esi,0x75bbf934
824E5A: add esi,0x007bd865
824E60: push eax
824E61: mov eax,0x7cfb514f
824E66: sub esi,eax
824E68: pop eax
824E69: add esi,0x0703f3cf
824E6F: mov ebp,esi
824E71: mov esi,dword ds:[esp]
824E74: add esp,04
824E77: add dword ds:[esp+0x04],0x7ffdaa13
824E7F: sub dword ds:[esp+0x04],ebp
824E83: sub dword ds:[esp+0x04],0x7ffdaa13
824E8B: pop ebp
824E8C: push dword ds:[esp]
824E8F: push dword ds:[esp]
824E92: pop ebx
824E93: add esp,00000004
824E99: add esp,00000004
824E9F: mov dword ds:[ebp+ebx+0x00],0x6f7ec0c0
824EA7: xor dword ds:[ebp+ebx+0x00],0x7f9be38b
824EAF: push ecx
824EB0: mov ecx,0x75bb2160
824EB5: xor ecx,0x7afe34aa
824EBB: neg ecx
824EBD: push eax
824EBE: mov eax,0x7f3bb600
824EC3: and ecx,eax
824EC5: pop eax
824EC6: inc ecx
824EC7: neg ecx
824EC9: sub ecx,0x15ce13c9
824ECF: or dword ds:[ebp+ebx+0x00],ecx
824ED3: pop ecx
824ED4: push edi
824ED5: mov edi,0xffffffff
824EDA: add dword ds:[ebp+ebx+0x00],edi
824EDE: pop edi
824EDF: push ecx
824EE0: mov dword ds:[esp],eax
824EE3: push edx
824EE4: mov edx,ebx
824EE6: mov eax,edx
824EE8: pop edx
824EE9: push esi
824EEA: mov esi,0x78fff5dc
824EEF: xor esi,0x61df3ad9
824EF5: not esi
824EF7: inc esi
824EF8: and esi,0x373f69d7
824EFE: sub esi,0x7f9fbaa5
824F04: add esi,0xc5748933
824F0A: xor esi,0x2f370994
824F10: not esi
824F12: push edx
824F13: mov edx,0x67f7c96d
824F18: sub edx,0xff
824F1B: dec edx
824F1C: xor edx,0x14aadc08
824F22: push ecx
824F23: mov ecx,0xec984a9f
824F28: add edx,ecx
824F2A: pop ecx
824F2B: xor esi,edx
824F2D: pop edx
824F2E: neg esi
824F30: shl esi,01
824F33: add esi,0xc99cf21c
824F39: add eax,esi
824F3B: pop esi
824F3C: add eax,ebp
824F3E: xor dword ds:[eax],0x70b786f8
824F44: push dword ds:[esp]
824F47: pop eax
824F48: add esp,04
824F4B: push esi
824F4C: push 0x0900ed86
824F51: pop esi
824F52: sub dword ds:[ebp+ebx+0x00],esi
824F56: mov esi,dword ds:[esp]
824F59: add esp,04
824F5C: mov ebx,0x7f5f8b24
824F61: or ebx,0x43b58697
824F67: add ebx,0x5bfb0cea
824F6D: xchg ebx,esi
824F6F: push esi
824F70: push ebx
824F71: mov esi,dword ds:[esp]
824F74: add esp,04
824F77: pop ebx
824F78: not ebx
824F7A: push esi
824F7B: push ebx
824F7C: pop esi
824F7D: pop ebx
824F7E: xchg ebx,esi
824F80: push ebp
824F81: mov dword ds:[esp],0x2aa6fb74
824F88: mov dword ds:[esp],edi
824F8B: mov edi,0x58bd2eba
824F90: sub ebx,0x7bf2a884
824F96: add ebx,edi
824F98: add ebx,0x7bf2a884
824F9E: pop edi
824F9F: push ebp
824FA0: mov ebp,0x5b5ed720
824FA5: push edx
824FA6: mov edx,0x47ef9027
824FAB: add edx,0x35eff74e
824FB1: or ebp,edx
824FB3: pop edx
824FB4: xor ebp,0x6f7efc9b
824FBA: shr ebp,03
824FBD: push eax
824FBE: mov dword ds:[esp],ebp
824FC1: xor dword ds:[esp],edx
824FC4: xor edx,dword ds:[esp]
824FC7: xor dword ds:[esp],edx
824FCA: not edx
824FCC: xchg dword ds:[esp],edx
824FCF: mov ebp,dword ds:[esp]
824FD2: push edx
824FD3: mov edx,esp
824FD5: push eax
824FD6: mov eax,00000004
824FDB: add edx,eax
824FDD: mov eax,dword ds:[esp]
824FE0: add esp,00000004
824FE6: add edx,00000004
824FEC: xchg dword ds:[esp],edx
824FEF: pop esp
824FF0: xor ebp,0x81294882
824FF6: xor ebx,ebp
824FF8: pop ebp
824FF9: push dword ds:[esp+0x28]
824FFD: push dword ds:[esp]
825000: pop eax
825001: sub esp,04
825004: mov dword ds:[esp],edx
825007: push 0x74f0f16d
82500C: push esp
82500D: pop dword ds:[esp]
825010: add dword ds:[esp],04
825014: mov edx,dword ds:[esp]
825017: push ecx
825018: push esp
825019: pop ecx
82501A: push ebp
82501B: mov ebp,00000004
825020: add ecx,0x5f7399e2
825026: add ecx,ebp
825028: sub ecx,0x5f7399e2
82502E: pop ebp
82502F: add ecx,00000004
825035: xchg dword ds:[esp],ecx
825038: pop esp
825039: add edx,00000004
82503F: add edx,04
825042: push 0x412e3ac6
825047: mov dword ds:[esp],edx
82504A: push dword ds:[esp+0x04]
82504E: push dword ds:[esp]
825051: pop edx
825052: push edi
825053: mov edi,esp
825055: add edi,00000004
82505B: add edi,00000004
825061: xchg dword ds:[esp],edi
825064: mov esp,dword ds:[esp]
825067: pop dword ds:[esp]
82506A: mov esp,dword ds:[esp]
82506D: add eax,0x4bfa2258
825072: sub eax,0x7dffca01
825077: add eax,ecx
825079: add eax,0x7dffca01
82507E: push esi
82507F: mov esi,0x67dd089a
825084: push ebx
825085: mov ebx,00000001
82508A: add esi,0x7f7dcb0d
825090: sub esi,ebx
825092: sub esi,0x7f7dcb0d
825098: pop ebx
825099: add esi,0x19cd4cef
82509F: push ebx
8250A0: mov ebx,0xffffffff
8250A5: sub esi,ebx
8250A7: pop ebx
8250A8: sub esp,00000004
8250AE: mov dword ds:[esp],ecx
8250B1: mov ecx,00000000
8250B6: sub ecx,0x0fff3560
8250BC: sub ecx,esi
8250BE: push esi
8250BF: mov esi,0x5df94daf
8250C4: inc esi
8250C5: and esi,0x7bf92d37
8250CB: xor esi,0x7cdb15e5
8250D1: sub esi,0x1522e375
8250D7: add ecx,esi
8250D9: pop esi
8250DA: push 0x003e6437
8250DF: mov dword ds:[esp],ecx
8250E2: xor dword ds:[esp],0x79ffa429
8250E9: mov esi,dword ds:[esp]
8250EC: add esp,00000004
8250F2: xor esi,0x79ffa429
8250F8: push dword ds:[esp]
8250FB: pop ecx
8250FC: add esp,00000004
825102: push ecx
825103: push eax
825104: mov eax,0x35af882f
825109: mov ecx,eax
82510B: pop eax
82510C: xor esi,ecx
82510E: pop ecx
82510F: sub eax,esi
825111: mov esi,dword ds:[esp]
825114: add esp,00000004
82511A: push eax
82511B: mov eax,esp
82511D: push ecx
82511E: mov ecx,0x3ae95c47
825123: dec ecx
825124: dec ecx
825125: sub ecx,0x3aae6241
82512B: shr ecx,07
82512E: add ecx,0xffff8a10
825134: add eax,ecx
825136: pop ecx
825137: sub eax,00000004
82513C: xchg dword ds:[esp],eax
82513F: pop esp
825140: mov dword ds:[esp],edx
825143: sub esp,04
825146: mov dword ds:[esp],eax
825149: sub dword ds:[esp],0x7ffea87c
825150: push dword ds:[esp]
825153: mov edx,dword ds:[esp]
825156: add esp,04
825159: add esp,00000004
82515F: push 0x73c8d919
825164: mov dword ds:[esp],ebx
825167: push ecx
825168: sub esp,00000004
82516E: mov dword ds:[esp],eax
825171: push ebp
825172: mov ebp,0x767d4e7f
825177: mov eax,ebp
825179: mov ebp,dword ds:[esp]
82517C: add esp,00000004
825182: add eax,0x7fed39fb
825187: sub eax,0xa36f530d
82518C: mov ecx,eax
82518E: mov eax,dword ds:[esp]
825191: add esp,04
825194: add ecx,0x6f3fad94
82519A: and ecx,0x7bff6343
8251A0: xor ecx,0x3d51f7fd
8251A6: mov ebx,ecx
8251A8: mov ecx,dword ds:[esp]
8251AB: add esp,04
8251AE: not ebx
8251B0: inc ebx
8251B1: push eax
8251B2: mov eax,0xff6a3d78
8251B7: add ebx,eax
8251B9: pop eax
8251BA: add edx,ebx
8251BC: pop ebx
8251BD: mov dword ds:[ebp+ebx+0x00],edx
8251C1: mov edx,dword ds:[esp]
8251C4: add esp,00000004
8251CA: mov eax,0x47f9b883
8251CF: push edi
8251D0: mov edi,0x4fefaa1a
8251D5: shl edi,03
8251D8: add edi,0xff
8251DB: not edi
8251DD: push ebx
8251DE: mov ebx,0x3b7dbd00
8251E3: push edi
8251E4: mov edi,0x5bed377b
8251E9: and edi,0x7ffb27cf
8251EF: add edi,0x18166a91
8251F5: and ebx,edi
8251F7: pop edi
8251F8: dec ebx
8251F9: add ebx,0xac7e7b6c
8251FF: xor edi,ebx
825201: pop ebx
825202: xor eax,edi
825204: mov edi,dword ds:[esp]
825207: push edx
825208: mov edx,esp
82520A: add edx,00000004
825210: add edx,04
825213: push edx
825214: push dword ds:[esp+0x04]
825218: pop edx
825219: pop dword ds:[esp]
82521C: mov esp,dword ds:[esp]
82521F: dec eax
825220: push ebp
825221: mov ebp,0xe7ba480a
825226: push 0x4f0aa5b7
82522B: mov dword ds:[esp],edx
82522E: mov edx,0x6ef69999
825233: inc edx
825234: shl edx,08
825237: or edx,0x7ebc7d0a
82523D: shl edx,02
825240: inc edx
825241: sub edx,0xdc98dfe3
825247: push edx
825248: mov edx,0x67ff269b
82524D: sub edx,0x7b7fee06
825253: and edx,0x7db76e8d
825259: neg edx
82525B: inc edx
82525C: and edx,0x7f707169
825262: add edx,0x66bb7c2a
825268: sub eax,edx
82526A: pop edx
82526B: sub eax,edx
82526D: add eax,0x79fbcd92
825272: pop edx
825273: add eax,ebp
825275: push ecx
825276: mov ecx,0x1e5f1c46
82527B: add eax,0x7f6a0462
825280: sub eax,0x7def48b1
825285: add eax,ecx
825287: add eax,0x7def48b1
82528C: sub eax,0x7f6a0462
825291: pop ecx
825292: pop ebp
825293: push edx
825294: mov edx,0x7ff8d8dc
825299: neg edx
82529B: or edx,0x7fc762b5
8252A1: shr edx,03
8252A4: dec edx
8252A5: xor edx,0x606bc6bb
8252AB: add eax,edx
8252AD: pop edx
8252AE: sub eax,0x7edad721
8252B3: add eax,ecx
8252B5: push eax
8252B6: mov dword ds:[esp],ebp
8252B9: mov ebp,0x7edad721
8252BE: add eax,ebp
8252C0: mov ebp,dword ds:[esp]
8252C3: add esp,00000004
8252C9: push 0x094ccf07
8252CE: mov dword ds:[esp],ebx
8252D1: mov dword ds:[esp],ebp
8252D4: mov ebp,0x6efb9345
8252D9: shl ebp,03
8252DC: shl ebp,03
8252DF: shl ebp,02
8252E2: sub ebp,01
8252E5: shl ebp,04
8252E8: push edx
8252E9: mov edx,0x1fbf3051
8252EE: push ebp
8252EF: mov ebp,0x7d7f3abf
8252F4: or edx,ebp
8252F6: pop ebp
8252F7: add edx,0xb9a1eaa3
8252FD: sub ebp,0x7ff675a8
825303: add ebp,0x5dff11d5
825309: sub ebp,edx
82530B: sub ebp,0x5dff11d5
825311: add ebp,0x7ff675a8
825317: pop edx
825318: add eax,0x7fbd4136
82531D: sub eax,ebp
82531F: push ebx
825320: push eax
825321: push 0x5ff9f663
825326: pop eax
825327: and eax,0x7c1f55ca
82532C: add eax,0xc365ffff
825331: mov ebx,eax
825333: pop eax
825334: push edx
825335: mov dword ds:[esp],ecx
825338: mov ecx,0x603decf5
82533D: add ebx,ecx
82533F: mov ecx,dword ds:[esp]
825342: add esp,00000004
825348: sub eax,ebx
82534A: pop ebx
82534B: pop ebp
82534C: mov ebx,0x7f9fed24
825351: push 0x5234c32b
825356: mov dword ds:[esp],ebx
825359: not dword ds:[esp]
82535C: pop ebx
82535D: push esi
82535E: push ecx
82535F: mov ecx,0x7df6b29a
825364: mov esi,ecx
825366: pop ecx
825367: sub esi,0x7ff6b1ee
82536D: and esi,0x7fbba7b6
825373: neg esi
825375: not esi
825377: sub esi,0x11817dd9
82537D: push ecx
82537E: mov ecx,0x2fc9b154
825383: sub esi,0x6d7fbe00
825389: sub esi,ecx
82538B: push ebx
82538C: mov ebx,0x2ffea59e
825391: and ebx,0x4fb95038
825397: not ebx
825399: shl ebx,04
82539C: add ebx,0x68ffbf90
8253A2: add esi,ebx
8253A4: pop ebx
8253A5: pop ecx
8253A6: inc esi
8253A7: or esi,0x4fe7d40e
8253AD: add esi,0x7fdefb7d
8253B3: sub esi,0x3af24ac1
8253B9: push eax
8253BA: mov eax,0x6fd7e6af
8253BF: dec eax
8253C0: shr eax,03
8253C3: push ebx
8253C4: push ecx
8253C5: mov ecx,0x375fb990
8253CA: mov ebx,0x59b81552
8253CF: xor ebx,ecx
8253D1: pop ecx
8253D2: add ebx,0x00cbf649
8253D8: or eax,ebx
8253DA: pop ebx
8253DB: push ecx
8253DC: mov ecx,0x1fdfb718
8253E1: and eax,ecx
8253E3: pop ecx
8253E4: xor eax,0xd1601ef8
8253E9: xor esi,eax
8253EB: pop eax
8253EC: or ebx,esi
8253EE: pop esi
8253EF: shr ebx,03
8253F2: push ecx
8253F3: push edx
8253F4: mov edx,0x7af30de6
8253F9: mov ecx,0x96431574
8253FE: xor ecx,edx
825400: pop edx
825401: add ebx,ecx
825403: pop ecx
825404: push dword ds:[ebp+ebx+0x00]
825408: add dword ds:[esp],0x1fff609a
82540F: push dword ds:[esp]
825412: pop edx
825413: sub esp,00000004
825419: mov dword ds:[esp],eax
82541C: mov dword ds:[esp],edi
82541F: mov edi,esp
825421: add edi,00000004
825427: push edx
825428: mov dword ds:[esp],eax
82542B: push ecx
82542C: mov ecx,0x37ff931b
825431: or ecx,0x6de96db2
825437: shr ecx,05
82543A: add ecx,0x73ceabaf
825440: mov eax,ecx
825442: pop ecx
825443: add eax,0x7defaa1f
825448: add eax,0x7baac62e
82544D: inc eax
82544E: add eax,0x77e79417
825453: sub eax,0x7b7993b6
825458: sub eax,0x6dd71c57
82545D: add edi,0x7ffa7f24
825463: add edi,eax
825465: sub edi,0x7ffa7f24
82546B: pop eax
82546C: xchg dword ds:[esp],edi
82546F: mov esp,dword ds:[esp]
825472: push edx
825473: push esp
825474: pop edx
825475: add edx,00000004
82547B: sub edx,04
82547E: xchg dword ds:[esp],edx
825481: pop esp
825482: mov dword ds:[esp],ebp
825485: mov ebp,0x1fff609a
82548A: sub edx,ebp
82548C: push dword ds:[esp]
82548F: pop ebp
825490: push edx
825491: mov edx,esp
825493: add edx,00000004
825499: add edx,00000004
82549F: xchg dword ds:[esp],edx
8254A2: mov esp,dword ds:[esp]
8254A5: cmp edx,eax
8254A7: jz  0x00825a48
8254AD: push 0x0d8154e1
8254B2: mov dword ds:[esp],eax
8254B5: mov eax,esp
8254B7: add eax,00000004
8254BC: sub esp,04
8254BF: mov dword ds:[esp],ebx
8254C2: push edx
8254C3: mov edx,0x4b7e61a5
8254C8: push ecx
8254C9: mov ecx,0x63fcb7e8
8254CE: push edi
8254CF: mov edi,0x55c112ad
8254D4: sub ecx,edi
8254D6: pop edi
8254D7: xor ecx,0x51973948
8254DD: and edx,ecx
8254DF: pop ecx
8254E0: shl edx,03
8254E3: inc edx
8254E4: shr edx,03
8254E7: push ebx
8254E8: mov ebx,0x3d1fc257
8254ED: xor edx,ebx
8254EF: pop ebx
8254F0: sub edx,0xe633d48a
8254F6: mov ebx,edx
8254F8: pop edx
8254F9: inc ebx
8254FA: shl ebx,03
8254FD: push edx
8254FE: push 0x7fecd261
825503: pop edx
825504: inc edx
825505: shl edx,03
825508: push eax
825509: push edi
82550A: mov edi,0x6afff26c
82550F: not edi
825511: inc edi
825512: xor edi,0xc9f60ca5
825518: mov eax,edi
82551A: pop edi
82551B: push edx
82551C: mov edx,0x5b266533
825521: xor edx,0x877b41e6
825527: sub eax,edx
825529: pop edx
82552A: add edx,0x6fff116a
825530: add edx,eax
825532: sub edx,0x6fff116a
825538: mov eax,dword ds:[esp]
82553B: add esp,04
82553E: xor ebx,edx
825540: pop edx
825541: sub eax,ebx
825543: pop ebx
825544: push eax
825545: push dword ds:[esp+0x04]
825549: pop eax
82554A: pop dword ds:[esp]
82554D: pop esp
82554E: mov dword ds:[esp],ebx
825551: push edx
825552: mov edx,esp
825554: add edx,00000004
82555A: push ecx
82555B: push edi
82555C: mov edi,0x77fe242e
825561: or edi,0x7b5fa6a8
825567: add edi,0x7fff9e72
82556D: inc edi
82556E: dec edi
82556F: add edi,0x6fc85698
825575: mov ecx,edi
825577: pop edi
825578: neg ecx
82557A: sub ecx,0xff
82557D: not ecx
82557F: sub ecx,0x6fc79bb2
825585: sub edx,ecx
825587: pop ecx
825588: xor edx,dword ds:[esp]
82558B: xor dword ds:[esp],edx
82558E: xor edx,dword ds:[esp]
825591: pop esp
825592: mov dword ds:[esp],ecx
825595: push esi
825596: mov esi,0x1ff92df3
82559B: inc esi
82559C: add esi,0x7db5ec71
8255A2: not esi
8255A4: push ebp
8255A5: mov ebp,0x5ed78f45
8255AA: xor esi,ebp
8255AC: push dword ds:[esp]
8255AF: push dword ds:[esp]
8255B2: pop ebp
8255B3: push edx
8255B4: push esp
8255B5: pop edx
8255B6: add edx,00000004
8255BC: add edx,00000004
8255C2: xchg dword ds:[esp],edx
8255C5: pop esp
8255C6: add esp,00000004
8255CC: shr esi,08
8255CF: push edi
8255D0: mov edi,0x003c8252
8255D5: sub esi,0x7f7721cf
8255DB: sub esi,edi
8255DD: add esi,0x7f7721cf
8255E3: pop edi
8255E4: push 0x44f1948c
8255E9: mov dword ds:[esp],ecx
8255EC: mov dword ds:[esp],esi
8255EF: pop ecx
8255F0: pop esi
8255F1: mov ebx,ecx
8255F3: pop ecx
8255F4: shr ebx,02
8255F7: push ebp
8255F8: push esp
8255F9: pop ebp
8255FA: push 0x24b7b8dc
8255FF: mov dword ds:[esp],ecx
825602: push edi
825603: mov edi,0x2ffe7a01
825608: push edi
825609: add dword ds:[esp],0x7ff68047
825610: pop ecx
825611: push esi
825612: mov esi,0x69dfd336
825617: sub esi,0x7fe1c5cf
82561D: sub esi,0x6a078d20
825623: sub ecx,esi
825625: mov esi,dword ds:[esp]
825628: add esp,04
82562B: pop edi
82562C: push esi
82562D: mov esi,esp
82562F: add esi,00000004
825635: sub esi,04
825638: xchg dword ds:[esp],esi
82563B: pop esp
82563C: mov dword ds:[esp],eax
82563F: push edx
825640: mov edx,0x7d799b56
825645: mov eax,0x028fb056
82564A: xor eax,edx
82564C: pop edx
82564D: or ecx,eax
82564F: pop eax
825650: not ecx
825652: sub ecx,0x1f430213
825658: inc ecx
825659: sub ecx,0x60be82e8
82565F: sub ebp,0x1fb780c8
825665: add ebp,0x7b6fc392
82566B: add ebp,ecx
82566D: sub ebp,0x7b6fc392
825673: add ebp,0x1fb780c8
825679: mov ecx,dword ds:[esp]
82567C: add esp,04
82567F: push edi
825680: mov edi,00000004
825685: sub ebp,edi
825687: pop edi
825688: xor ebp,dword ds:[esp]
82568B: xor dword ds:[esp],ebp
82568E: xor ebp,dword ds:[esp]
825691: pop esp
825692: mov dword ds:[esp],eax
825695: mov dword ds:[esp],ebp
825698: mov dword ds:[esp],eax
82569B: test ebx,ebx
82569D: jz  0x008258a4
8256A3: sub esp,04
8256A6: mov dword ds:[esp],edx
8256A9: mov edx,00000000
8256AE: add edx,0x7f4f43df
8256B4: push esi
8256B5: mov dword ds:[esp],eax
8256B8: mov eax,0x7f7f2034
8256BD: xor eax,0x134e8cf4
8256C2: sub edx,0x7e7b5241
8256C8: sub edx,eax
8256CA: push ecx
8256CB: mov ecx,0x117eb2ef
8256D0: push esi
8256D1: mov esi,0x7df5c96c
8256D6: add esi,0xf1101742
8256DC: xor ecx,esi
8256DE: mov esi,dword ds:[esp]
8256E1: add esp,00000004
8256E7: add edx,0x2fcfa030
8256ED: add edx,0x3ff32004
8256F3: add edx,ecx
8256F5: sub edx,0x3ff32004
8256FB: push ebp
8256FC: mov ebp,0x7d4e1d54
825701: sub ebp,0x3b1e834e
825707: add ebp,0x7fc19054
82570D: neg ebp
82570F: push 0x4697183d
825714: mov dword ds:[esp],esi
825717: mov esi,00000000
82571C: sub esi,ebp
82571E: mov ebp,esi
825720: pop esi
825721: xor ebp,0xee3e8a6a
825727: sub edx,ebp
825729: pop ebp
82572A: mov ecx,dword ds:[esp]
82572D: add esp,04
825730: mov eax,dword ds:[esp]
825733: add esp,04
825736: add edx,eax
825738: add edx,0x6c31acc0
82573E: sub edx,0x7f4f43df
825744: add dword ds:[edx],ecx
825746: mov edx,dword ds:[esp]
825749: push ecx
82574A: mov ecx,esp
82574C: add ecx,00000004
825752: add ecx,04
825755: xchg dword ds:[esp],ecx
825758: mov esp,dword ds:[esp]
82575B: push edx
82575C: mov edx,0x6bfdcdb9
825761: push ebx
825762: mov ebx,0x77e5aae0
825767: xor edx,ebx
825769: pop ebx
82576A: add edx,0xffffffff
825770: add edx,00000001
825776: shr edx,03
825779: push ebp
82577A: mov dword ds:[esp],ecx
82577D: push edi
82577E: mov edi,0x3ce76e57
825783: mov ecx,edi
825785: mov edi,dword ds:[esp]
825788: add esp,04
82578B: dec ecx
82578C: push 0x0b030dfc
825791: mov dword ds:[esp],ebp
825794: mov ebp,00000001
825799: add ecx,ebp
82579B: pop ebp
82579C: push edi
82579D: mov edi,0x7dec7aac
8257A2: inc edi
8257A3: neg edi
8257A5: sub edi,0xffffffff
8257AB: add edi,0xfdbc0643
8257B1: sub ecx,0x7be72eae
8257B7: sub ecx,edi
8257B9: add ecx,0x7be72eae
8257BF: pop edi
8257C0: xor ecx,0xbe94ee2f
8257C6: xor edx,ecx
8257C8: pop ecx
8257C9: add eax,edx
8257CB: mov edx,dword ds:[esp]
8257CE: push 0x50a63511
8257D3: mov dword ds:[esp],esi
8257D6: mov dword ds:[esp],ecx
8257D9: mov ecx,esp
8257DB: add ecx,00000004
8257E1: push esi
8257E2: mov esi,00000004
8257E7: add ecx,esi
8257E9: pop esi
8257EA: xchg dword ds:[esp],ecx
8257ED: pop esp
8257EE: push ebx
8257EF: push 0x091c979d
8257F4: mov dword ds:[esp],edx
8257F7: push dword ds:[esp]
8257FA: pop ebx
8257FB: push 0x545bb2b3
825800: mov dword ds:[esp],esi
825803: mov esi,esp
825805: add esi,00000004
82580B: push edi
82580C: push esi
82580D: mov esi,0x7fa5816e
825812: mov edi,esi
825814: pop esi
825815: add edi,0x805a7e96
82581B: add esi,edi
82581D: pop edi
82581E: xchg dword ds:[esp],esi
825821: mov esp,dword ds:[esp]
825824: pop edx
825825: push 0x2eb7ca05
82582A: mov dword ds:[esp],edi
82582D: push ecx
82582E: push eax
82582F: mov eax,0x7afec400
825834: sub eax,0x5b5fb6e7
825839: add eax,0xffffffff
82583E: and eax,0x52bf4000
825843: neg eax
825845: not eax
825847: sub eax,0xa337c4ed
82584C: mov ecx,eax
82584E: pop eax
82584F: push ebp
825850: mov ebp,0x3fdf292e
825855: not ebp
825857: shr ebp,03
82585A: inc ebp
82585B: xor ebp,0x072e30b5
825861: and ecx,ebp
825863: pop ebp
825864: or ecx,0x5f670664
82586A: push ebp
82586B: mov ebp,0x7deec868
825870: neg ebp
825872: sub ebp,0x52cfd27a
825878: shr ebp,06
82587B: add ebp,0x7ddee1b3
825881: and ecx,ebp
825883: pop ebp
825884: or ecx,0x3ddc5293
82588A: push esi
82588B: mov esi,0x80208928
825890: xor ecx,esi
825892: pop esi
825893: mov edi,ecx
825895: pop ecx
825896: add edx,edi
825898: pop edi
825899: xor ebx,edx
82589B: xor edx,ebx
82589D: xor ebx,edx
82589F: jmp  0x0082569b
8258A4: push dword ds:[esp]
8258A7: push dword ds:[esp]
8258AA: pop eax
8258AB: add esp,04
8258AE: sub esp,04
8258B1: mov dword ds:[esp],eax
8258B4: mov eax,esp
8258B6: push ecx
8258B7: mov ecx,00000004
8258BC: add eax,ecx
8258BE: push dword ds:[esp]
8258C1: pop ecx
8258C2: push edx
8258C3: mov edx,esp
8258C5: add edx,00000004
8258CB: add edx,04
8258CE: xchg dword ds:[esp],edx
8258D1: pop esp
8258D2: push 0x35825903
8258D7: mov dword ds:[esp],ebp
8258DA: push ecx
8258DB: mov dword ds:[esp],ebx
8258DE: mov dword ds:[esp],0x7ffbc71f
8258E5: xor dword ds:[esp],0x6377fc7b
8258EC: not dword ds:[esp]
8258EF: sub dword ds:[esp],01
8258F3: and dword ds:[esp],0x3bff8841
8258FA: dec dword ds:[esp]
8258FD: add dword ds:[esp],0xdc8c8005
825904: pop ebp
825905: sub eax,0x5feb0f7d
82590A: add eax,ebp
82590C: add eax,0x5feb0f7d
825911: pop ebp
825912: push eax
825913: push dword ds:[esp+0x04]
825917: pop eax
825918: pop dword ds:[esp]
82591B: mov esp,dword ds:[esp]
82591E: push dword ds:[esp]
825921: pop ebx
825922: push eax
825923: mov eax,esp
825925: push ebx
825926: mov dword ds:[esp],edx
825929: push eax
82592A: mov dword ds:[esp],0x33157800
825931: mov dword ds:[esp],esi
825934: mov esi,0x3dfd29a3
825939: mov edx,0x42245fea
82593E: xor edx,esi
825940: push dword ds:[esp]
825943: pop esi
825944: add esp,00000004
82594A: push 00000000
82594F: sub dword ds:[esp],0x7d6fe0dc
825956: sub dword ds:[esp],edx
825959: push edx
82595A: mov edx,0x7d6fe0dc
82595F: add dword ds:[esp+0x04],edx
825963: mov edx,dword ds:[esp]
825966: add esp,04
825969: pop edx
82596A: and edx,0x463edf2e
825970: add edx,01
825973: sub edx,0x6ff8c206
825979: push ebp
82597A: push eax
82597B: mov eax,0x7fc52691
825980: mov ebp,0xef975f74
825985: sub ebp,eax
825987: pop eax
825988: add edx,ebp
82598A: pop ebp
82598B: add eax,edx
82598D: mov edx,dword ds:[esp]
825990: add esp,00000004
825996: add eax,04
825999: xor eax,dword ds:[esp]
82599C: xor dword ds:[esp],eax
82599F: xor eax,dword ds:[esp]
8259A2: mov esp,dword ds:[esp]
8259A5: push edi
8259A6: mov dword ds:[esp],eax
8259A9: push edx
8259AA: mov edx,0x42fdc703
8259AF: add dword ds:[esp+0x04],0x13f7fa34
8259B7: sub dword ds:[esp+0x04],edx
8259BB: sub dword ds:[esp+0x04],0x13f7fa34
8259C3: pop edx
8259C4: pop dword ds:[ebp+ebx+0x00]
8259C8: push eax
8259C9: push edi
8259CA: mov dword ds:[esp],0x7cdf7f27
8259D1: xor dword ds:[esp],0x572f66ec
8259D8: mov dword ds:[esp],ebx
8259DB: push ecx
8259DC: mov ecx,0x36bc8d9a
8259E1: xor dword ds:[esp+0x04],ecx
8259E5: pop ecx
8259E6: pop eax
8259E7: push ebx
8259E8: push edi
8259E9: mov edi,0x595f6901
8259EE: mov ebx,0xccf6a77d
8259F3: sub ebx,edi
8259F5: pop edi
8259F6: sub ebx,0x6fdf6393
8259FC: dec ebx
8259FD: inc ebx
8259FE: shr ebx,01
825A01: not ebx
825A03: push 0x260ab99e
825A08: mov dword ds:[esp],esi
825A0B: mov esi,0xc76784f1
825A10: sub ebx,esi
825A12: pop esi
825A13: xor eax,ebx
825A15: pop ebx
825A16: push 0x27773863
825A1B: mov dword ds:[esp],edx
825A1E: mov edx,00000000
825A23: add eax,0x7fffae20
825A28: add eax,edx
825A2A: push edx
825A2B: mov dword ds:[esp],ecx
825A2E: mov ecx,0x7fffae20
825A33: sub eax,ecx
825A35: pop ecx
825A36: pop edx
825A37: add eax,ebp
825A39: add dword ds:[eax],0x42fdc703
825A3F: mov eax,dword ds:[esp]
825A42: add esp,00000004
825A48: push dword ds:[esp+0x24]
825A4C: push dword ds:[esp]
825A4F: mov ebx,dword ds:[esp]
825A52: push ebx
825A53: push esp
825A54: pop ebx
825A55: add ebx,00000004
825A5B: add ebx,04
825A5E: push ebx
825A5F: push dword ds:[esp+0x04]
825A63: pop ebx
825A64: pop dword ds:[esp]
825A67: pop esp
825A68: push eax
825A69: mov dword ds:[esp],0x07bd4a00
825A70: mov dword ds:[esp],esi
825A73: mov dword ds:[esp],ecx
825A76: push esp
825A77: push dword ds:[esp]
825A7A: mov ecx,dword ds:[esp]
825A7D: push ebp
825A7E: mov ebp,esp
825A80: add ebp,00000004
825A86: add ebp,00000004
825A8C: xchg dword ds:[esp],ebp
825A8F: pop esp
825A90: add esp,04
825A93: add ecx,00000004
825A99: push ebx
825A9A: mov ebx,0x1ff5dec6
825A9F: push eax
825AA0: mov eax,0x1ff5dec2
825AA5: sub ebx,eax
825AA7: pop eax
825AA8: add ecx,0x3ddffbc1
825AAE: add ecx,ebx
825AB0: sub ecx,0x3ddffbc1
825AB6: pop ebx
825AB7: xor ecx,dword ds:[esp]
825ABA: xor dword ds:[esp],ecx
825ABD: xor ecx,dword ds:[esp]
825AC0: pop esp
825AC1: shl ebx,02
825AC4: push ecx
825AC5: mov dword ds:[esp],ebx
825AC8: sub esp,04
825ACB: mov dword ds:[esp],ebx
825ACE: mov dword ds:[esp],0x0fddaddc
825AD5: pop ebx
825AD6: shr ebx,04
825AD9: push ebp
825ADA: push 0x7b48884b
825ADF: mov dword ds:[esp],esi
825AE2: mov dword ds:[esp],edi
825AE5: push eax
825AE6: mov eax,0x79f37a2f
825AEB: dec eax
825AEC: or eax,0x37f7231a
825AF1: add eax,0xf6f3d129
825AF6: mov edi,eax
825AF8: pop eax
825AF9: xchg edi,ecx
825AFB: push ecx
825AFC: not dword ds:[esp]
825AFF: pop ecx
825B00: xchg edi,ecx
825B02: sub edi,0x0b2e58a6
825B08: mov ebp,edi
825B0A: pop edi
825B0B: push ebx
825B0C: mov ebx,0x6fb72d28
825B11: xor ebp,ebx
825B13: pop ebx
825B14: add ebp,0x6fb36216
825B1A: sub ebx,ebp
825B1C: pop ebp
825B1D: sub eax,0x7a7cb55b
825B22: sub eax,0x74ff9ac1
825B27: sub eax,0x17ff330d
825B2C: sub eax,ebx
825B2E: add eax,0x17ff330d
825B33: sub esp,00000004
825B39: mov dword ds:[esp],edi
825B3C: push eax
825B3D: mov eax,0x7d386041
825B42: or eax,0x30d73381
825B47: xor eax,0x7bfd07d2
825B4C: sub eax,0x9102d952
825B51: mov edi,eax
825B53: pop eax
825B54: add eax,edi
825B56: pop edi
825B57: add eax,0x7a7cb55b
825B5C: pop ebx
825B5D: add eax,ebx
825B5F: add eax,0x7ef900ed
825B64: jmp dword ds:[eax]
