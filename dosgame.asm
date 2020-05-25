DATA SEGMENT

TEXT_WELCOME_1 DB 'CROSS-ZERO',10,13,'$'
TEXT_WELCOME_2 DB 'A - move left',10,13,'W - move up',10,13,'S - move down',10,13,'$'
TEXT_WELCOME_3 DB 'D - move right',10,13,'Q - exit programm ',10,13,'$'
TEXT_WELCOME_4 DB 'Authors: ',10,13,'PRESS ANY KEY TO START$'
TEXT_1_PLAYER DB 'move of the cross ',13,'$'
TEXT_2_PLAYER DB 'move of the zero ',13,'$'
TEXT_3 DB 'CROSS WINS $',10
TEXT_4 DB 'ZERO WINS $',10
TEXT_5 DB 'FRIENDSHIP WINS $',10
rejimGr DW ? ; 

KOORD_X_OL dw 41
KOORD_Y_OL dw 41

KOORD_X_N dw 41
KOORD_Y_N dw 41  

XL dw 0
XP dw 0
YL dw 0
YP dw 0
Chetchik dw 0;
FLAG dw 0
COLOR db 0
HOD dw 0  
DATA ENDS

CODE SEGMENT
ASSUME CS:CODE, DS:DATA

MAIN PROC FAR
    MOV AX, DATA
    MOV DS, AX
    CALL SAVE_REJIM ;
    CALL GRAPH_INIT 
    
    MOV DX, OFFSET TEXT_WELCOME_1
    MOV AH, 9
    INT 21H
    MOV DX, OFFSET TEXT_WELCOME_2
    MOV AH, 9
    INT 21H
    MOV DX, OFFSET TEXT_WELCOME_3
    MOV AH, 9
    INT 21H
    MOV DX, OFFSET TEXT_WELCOME_4
    MOV AH, 9
    INT 21H
    MOV AH, 7
    INT 21H
    CALL GRAPH_INIT
    CALL GAME_CLEAR_SCR
    CALL POLE
metMAIN:
    inc HOD
    MOV DX, OFFSET TEXT_1_PLAYER
    MOV AH, 9
    INT 21H
    CALL MOVE_KURSOR 
    CALL VIVOD_CROSS 
    CALL KOORD 
    mov COLOR,9 
    
    CALL Vertical
    CALL KOORD2
    CALL Nakl    ;diag 1
    CALL KOORD3
    CALL Nakl2   ;diag 2
    cmp FLAG,1
    je metEMDPROGR
    inc HOD
    
    MOV DX, OFFSET TEXT_2_PLAYER
    MOV AH, 9
    INT 21H
    CALL MOVE_KURSOR
    CALL VIVOD_ZERO
    CALL KOORD
    mov COLOR,10
    CALL Vertical
    CALL KOORD2 
    CALL Nakl 
    CALL KOORD3 
    CALL Nakl2 
    cmp FLAG,1
    je metEMDPROGR
    
    cmp HOD,100 
    je NECHIA
    
    loop metMAIN

metEMDPROGR:
    cmp COLOR,9
    je CROSSW 
    MOV DX, OFFSET TEXT_4 
    MOV AH, 9      
    INT 21H      
    MOV AH, 7   
    INT 21H   
    loop D

    CROSSW: 
    MOV DX, OFFSET TEXT_3
    MOV AH, 9
    INT 21H
    MOV AH, 7
    INT 21H
    loop D

    NECHIA: 
    MOV DX, OFFSET TEXT_5
    MOV AH, 9
    INT 21H
    MOV AH, 7
    INT 21H          

    D:   ;end game
    CALL GAME_CLEAR_SCR
    CALL OLD_GRAPF
    mov AX,4c00h
    int 21h
    ret
 MAIN ENDP

GAME_CLEAR_SCR PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    MOV AH, 6
    MOV AL, 0
    MOV BH, 0
    MOV CH, 0
    MOV CL, 0
    MOV DH, 255
    MOV DL, 255
    INT 10H
    MOV AH, 2
    MOV BH, 0
    MOV DH, 0
    MOV DL, 0
    INT 10H
    POP DX
    POP CX
    POP BX
    POP AX
    RET
GAME_CLEAR_SCR ENDP

GRAPH_INIT PROC    ;graphic mode
    MOV AH, 0
    MOV AL, 12H
    INT 10H
    RET
GRAPH_INIT ENDP

SAVE_REJIM PROC
    mov ah,0
    int 10h
    mov rejimGr,ax
    ret
SAVE_REJIM ENDP

OLD_GRAPF PROC       ;call old graphic mode
    PUSH AX
    mov ax,rejimGr
    mov ah,0
    int 10h
    POP AX
    ret
OLD_GRAPF ENDP

POLE PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    mov cx,40     ;color background
    mov dx,40
    mov al,2
    mov ah,0ch
    mov bh,0
  met11:
    int 10h
    inc dx
    cmp dx,440 
    jbe met11    
    
    mov dx,40
    add cx,1
    cmp cx,440
    jbe met11  
                 ;color lines
    mov cx,40
    mov dx,40
    mov al,15
  met1:
    mov ah,0ch
    mov bh,0
    int 10h
    inc dx
    cmp dx,440
    jbe met1
    mov dx,40
    add cx,8
    cmp cx,440
    jbe met1
    mov cx,0
    mov dx,0
    mov cx,40
    mov dx,40
  met2:
    mov ah,0ch
    mov bh,0
    int 10h
    inc cx
    cmp cx,440
    jbe met2
    mov cx,40
    add dx,8
    cmp dx,440
    jbe met2
    ;mov ax,0
    ;int 16h
    POP DX
    POP CX
    POP BX
    POP AX
    RET
POLE ENDP

RIC_KURSOR PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    mov cx,KOORD_X_OL
    mov dx,KOORD_Y_OL
    sub cx,1
    sub dx,1
    mov al,15
    mov ah,0ch
    mov bh,0
    mov Chetchik,0
  metResH2: 
    int 10h
    inc cx
    inc Chetchik
    cmp Chetchik,8
    jl metResH2
    mov Chetchik,0
  metResH3: 
    int 10h
    inc dx
    inc Chetchik
    cmp Chetchik,8
    jl metResH3
    mov Chetchik,0
  metResh4:
    int 10h
    sub dx,1
    inc Chetchik
    cmp Chetchik,8
    jl metResh4
    mov cx,KOORD_X_N
    mov dx,KOORD_Y_N
    mov Chetchik,0
    sub cx,1
    sub dx,1
    mov al,12
  metResH21: 
    int 10h
    inc cx
    inc Chetchik
    cmp Chetchik,8
    jl metResH21
    mov Chetchik,0
  metResH31: 
    int 10h
    inc dx
    inc Chetchik
    cmp Chetchik,8
    jl metResH31
    mov Chetchik,0
  metResh_31: 
    int 10h
    sub cx,1
    inc Chetchik
    cmp Chetchik,8
    jl metResh_31
    mov Chetchik,0
  metResh41: 
    int 10h
    sub dx,1
    inc Chetchik
    cmp Chetchik,8
    jl metResh41
    POP DX
    POP CX
    POP BX
    POP AX
    RET
RIC_KURSOR ENDP

VIVOD_A PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    cmp KOORD_X_OL,41
    je exitP
    mov ax, KOORD_X_OL
    sub ax,8
    mov KOORD_X_N,ax
    call RIC_KURSOR
    mov ax, KOORD_X_N
    mov KOORD_X_OL,ax
  exitP:
    POP DX
    POP CX
    POP BX
    POP AX
    RET
VIVOD_A ENDP

VIVOD_W PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    cmp KOORD_Y_OL,41
    je exitP1
    mov ax, KOORD_Y_OL
    sub ax,8
    mov KOORD_Y_N,ax
    call RIC_KURSOR
    mov ax, KOORD_Y_N
    mov KOORD_Y_OL,ax
  exitP1:
    POP DX
    POP CX
    POP BX
    POP AX
    RET
VIVOD_W ENDP

VIVOD_D PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    cmp KOORD_X_OL,433
    je exitP2
    mov ax, KOORD_X_OL
    add ax,8
    mov KOORD_X_N,ax
    call RIC_KURSOR
    mov ax, KOORD_X_N
    mov KOORD_X_OL,ax
  exitP2:
    POP DX
    POP CX
    POP BX
    POP AX
    RET
VIVOD_D ENDP

VIVOD_S PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    cmp KOORD_Y_OL,433
    je exitP3
    mov ax, KOORD_Y_OL
    add ax,8
    mov KOORD_Y_N,ax
    call RIC_KURSOR
    mov ax, KOORD_Y_N
    mov KOORD_Y_OL,ax
  exitP3:
    POP DX
    POP CX
    POP BX
    POP AX
    RET
VIVOD_S ENDP

MOVE_KURSOR PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    call RIC_KURSOR  ;start cursor
  metK1:
    mov ax,0
    mov ah,07
    int 21h
    cmp al,' '
    je endK
    cmp al,'a'
    je rica
    cmp al,'w'
    je ricw
    cmp al,'s'
    je rics
    cmp al,'d'
    je ricd
    cmp al,'q'
    je reakq
    loop metK1
  rica: 
    call VIVOD_A
    loop metK1
  ricw: 
    call VIVOD_W
    loop metK1
  rics:
    call VIVOD_S
    loop metK1
  ricd: 
    call VIVOD_D
    loop metK1
  reakq: 
    MOV DX, OFFSET TEXT_5
    MOV AH, 9
    INT 21H
    MOV AH, 7
    INT 21H
    CALL GAME_CLEAR_SCR
    CALL OLD_GRAPF
    mov AX,4c00h
    int 21h
    ret
;ret
  endK:
    mov al,0
    mov ah,0dh
    mov bh,0
    mov cx,KOORD_X_OL
    mov dx,KOORD_Y_OL
    int 10h
    cmp al,9
    je metK1
    cmp al,10
    je metK1
    POP DX
    POP CX
    POP BX
    POP AX
    RET
MOVE_KURSOR ENDP

VIVOD_CROSS PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    CALL Black_KV
    mov cx, KOORD_X_OL
    mov dx, KOORD_Y_OL
    mov al,9
    mov ah,0Ch
    mov bh,0
    mov Chetchik,0
  metKrest1:
    int 10h
    inc cx
    inc dx
    inc Chetchik
    cmp Chetchik,7
    jl metKrest1
    mov cx, KOORD_X_OL
    sub dx,1
    mov Chetchik,0
  metKrest2:
    int 10h
    inc cx
    sub dx,1
    inc Chetchik
    cmp Chetchik,7
    jl metKrest2
    mov cx, KOORD_X_OL
    mov dx, KOORD_Y_OL
    mov Chetchik,0
    inc cx
  metKrest3:
    int 10h
    inc cx
    inc dx
    inc Chetchik
    cmp Chetchik,6
    jl metKrest3
    mov cx, KOORD_X_OL
    inc cx
    mov Chetchik,0
  metKrest4:
    int 10h
    inc cx
    sub dx,1
    inc Chetchik
    cmp Chetchik,6
    jl metKrest4
    POP DX
    POP CX
    POP BX
    POP AX
    RET
VIVOD_CROSS ENDP

VIVOD_ZERO PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    CALL Black_KV
    mov cx, KOORD_X_OL
    mov dx, KOORD_Y_OL
    mov al,10
    mov ah,0Ch
    mov bh,0
    mov Chetchik,0
  metZero1:
    int 10h
    inc cx
    inc Chetchik
    cmp Chetchik,7
    jl metZero1
    mov cx, KOORD_X_OL
    inc dx
    mov Chetchik,0
  metZero2:
    int 10h
    inc cx
    inc Chetchik
    cmp Chetchik,7
    jl metZero2
    mov cx, KOORD_X_OL
    mov dx, KOORD_Y_OL
    mov Chetchik,0
    add dx,5
  metZero3:
    int 10h
    inc cx
    inc Chetchik
    cmp Chetchik,7
    jl metZero3
    mov cx, KOORD_X_OL
    inc dx
    mov Chetchik,0
  metZero4:
    int 10h
    inc cx
    inc Chetchik
    cmp Chetchik,7
    jl metZero4
    mov dx, KOORD_Y_OL
    add dx,2
    mov Chetchik,0
  metZero5:
    mov cx, KOORD_X_OL
    int 10h
    inc cx
    int 10h
    add cx,4
    int 10h
    inc cx
    int 10h
    inc Chetchik
    inc dx
    cmp Chetchik, 3
    jl metZero5
    POP DX
    POP CX
    POP BX
    POP AX
    RET
VIVOD_ZERO ENDP

Black_KV PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    mov al,6 
    mov ah,0ch
    mov bh,0
    mov Chetchik,0
    mov dx, KOORD_Y_OL
  metBlack1:
    mov cx, KOORD_X_OL
    int 10h ;
    inc cx
    int 10h ;
    inc cx
    int 10h ;
    inc cx
    int 10h ;
    inc cx
    int 10h ;
    inc cx
    int 10h ;
    inc cx
    int 10h ;
    inc dx
    inc Chetchik
    cmp Chetchik,7
    jl metBlack1
    POP DX
    POP CX
    POP BX
    POP AX
    RET
Black_KV ENDP

; cursor location

KOORD PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    mov ax,0
    mov ax,KOORD_X_OL
    mov Chetchik,0
  metXP:
    inc Chetchik
    cmp ax,433
    je next1
    cmp Chetchik,5
    je next1
    add ax,8
    loop metXP
  next1:
    mov XP,ax
    mov ax,0
    mov ax,KOORD_X_OL
    mov Chetchik,0
  metXL:
    inc Chetchik
    cmp ax,41
    je next2
    cmp Chetchik,5
    je next2
    sub ax,8
    loop metXL
  next2:
    mov XL,ax
    mov ax,0
    mov ax,KOORD_Y_OL
    mov Chetchik,0
  metYP: 
    inc Chetchik
    cmp ax,433
    je next3
    cmp Chetchik,5
    je next3
    add ax,8
    loop metYP
  next3:
    mov YP,ax
    mov ax,0
    mov ax,KOORD_Y_OL
    mov Chetchik,0
metYL:
    inc Chetchik
    cmp ax,41
    je next4
    cmp Chetchik,5
    je next4
    sub ax,8
    loop metYL
  next4:
    mov YL,ax
    POP DX
    POP CX
    POP BX
    POP AX
    RET
KOORD ENDP

Vertical PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    mov dx,YL
    mov cx,KOORD_X_OL
    mov ah,0dh
    mov bx,0
    mov Chetchik,0
  nachVP1: 
    mov al,0
    int 10h
    cmp al,COLOR
    je metGor1
    mov Chetchik,-1
  metGor1:
    inc Chetchik
    cmp Chetchik,5
    je END1
    add dx,8
    cmp dx,YP
    jbe nachVP1
    mov dx,0
    mov cx,0
    mov Chetchik,0
    mov cx, XL
    mov dx, KOORD_Y_OL
  nachVP2: 
    mov al,0
    int 10h
    cmp al, COLOR
    je metGor2
    mov Chetchik,-1
  metGor2:
    inc Chetchik
    cmp Chetchik,5
    je END1
    add cx,8
    cmp cx,XP
    jbe nachVP2
    POP DX
    POP CX
    POP BX
    POP AX
    RET
  END1:
    mov FLAG,1
    POP DX
    POP CX
    POP BX
    POP AX
    RET
Vertical ENDP
   ;diag 1
Nakl PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    mov ah,0dh
    mov bx,0
    mov Chetchik,0
    mov cx,0
    mov dx,0
    mov cx,XL
    mov dx,YL
    add XP,5
    add XL,5
  nachNakl1: 
    mov al, 0
    int 10h
    cmp al,COLOR
    je metNakl1
    mov Chetchik,-1
  metNakl1: 
    inc Chetchik
    cmp Chetchik,5
    je END_NAKL
    add cx,9
    add dx,8
    cmp cx,XP
    jg Prov2
    cmp dx,YP
    jg Prov2
    loop nachNakl1
  Prov2:
    POP DX
    POP CX
    POP BX
    POP AX
    RET
  END_NAKL:
    mov FLAG,1
    POP DX
    POP CX
    POP BX
    POP AX
    RET
Nakl ENDP

KOORD2 PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    mov ax,0
    mov bx,0
    mov ax,KOORD_X_OL
    mov bx,KOORD_Y_OL
    mov Chetchik,0
  mL: 
    inc Chetchik
    cmp ax,41
    je M1
    cmp bx,41
    je M1
    cmp Chetchik,5
    je M1
    sub ax,8
    sub bx,8
    loop mL
  M1:
    mov YL,bx
    mov XL,ax
    mov Chetchik,0
    mov ax,0
    mov bx,0
    mov ax,KOORD_X_OL
    mov bx,KOORD_Y_OL
  mP: 
    inc Chetchik
    cmp ax,433
    je M2
    cmp bx,433
    je M2
    cmp Chetchik,5
    je M2
    add ax,8
    add bx,8
    loop mP
  M2: 
    mov YP,bx
    mov XP,ax
    POP DX
    POP CX
    POP BX
    POP AX
    RET
KOORD2 ENDP

KOORD3 PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    mov ax,0
    mov bx,0
    mov ax,KOORD_X_OL
    mov bx,KOORD_Y_OL
    mov Chetchik,0
  mLL: 
    inc Chetchik
    cmp ax,433
    je MM1
    cmp bx,41
    je MM1
    cmp Chetchik,5
    je MM1
    add ax,8
    sub bx,8
    loop mLL
  MM1: 
    mov YL,bx
    mov XP,ax
    mov Chetchik,0
    mov ax,0
    mov bx,0
    mov ax,KOORD_X_OL
    mov bx,KOORD_Y_OL
  mPP: 
    inc Chetchik
    cmp ax,41
    je MM2
    cmp bx,433
    je MM2
    cmp Chetchik,5
    je MM2
    sub ax,8
    add bx,8
    loop mPP
  MM2: 
    mov YP, bx
    mov XL, ax
    POP DX
    POP CX
    POP BX
    POP AX
    RET
KOORD3 ENDP

;diag 2

Nakl2 PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    mov ah,0dh
    mov bx,0
    mov Chetchik,0
    mov cx,0
    mov dx,0
    mov cx,XP
    mov dx,YL
  nachNakll2:
    mov al,0
    int 10h
    cmp al,COLOR
    je metNakll1
    mov Chetchik,-1
  metNakll1:
    inc Chetchik
    cmp Chetchik,5
    je END_NAKLL
    sub cx,7
    add dx,8
    cmp cx,XL
    jle Provv2
    cmp dx,YP
    jg Provv2
    loop nachNakll2
  Provv2:
    POP DX
    POP CX
    POP BX
    POP AX
    RET
  END_NAKLL:
    mov FLAG,1
    POP DX
    POP CX
    POP BX
    POP AX
    RET
Nakl2 ENDP

CODE ENDS

END MAIN