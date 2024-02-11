    
    IDEAL
    
    MODEL small

    STACK 256
    JUMPS
    ASCnull     EQU 0
    TailLen     EQU 0080h       ;Offset of param len byte
    CommandTail EQU 0081h       ;Offset of parameters
    cr          EQU 13
    lf          EQU 10


    DATASEG
    inputFile   DW 0
    numParams   DW ?            ;Number of parameters
    lineCount   DW ?
    params      DB 256 DUP (?)  ;256-byte block for strings
    buff        DB 10 DUP (0)  ;8191
    buffsize    Dw 10
    excode DB 0
    helpstr DB '-h',ASCnull
    filArg DB '-k',ASCnull
    stra DB 'dadass',ASCnull
    Space DB ' ',ASCnull
    string DB 20 DUP (0)
    s1     DB 'Number of parameters = ', ASCnull
    HelpString DB 'with -k specify input file',13,10, ASCnull
    InvalidArgs DB 'Invalid arguments use -h for more info',13,10, ASCnull


    CODESEG
    INCLUDE "strings.asm"

PROC Separators
    mov al, [si]
    cmp al, 020h
    je @@10
    cmp al, 009h
    je @@10
    cmp al, 00Dh
@@10:
    ret
ENDP Separators

PROC ParamCount
    mov dx, [numParams]
    ret
ENDP ParamCount

PROC GetParams
;------Initialize counter (ex) and index registers (Si,di)
    xor ch, ch
    mov cl, [ds:TailLen]
    inc cx
    mov si, CommandTail
    mov di, offset params

@@10:
    call Separators
    jne @@20
    inc si
    loop @@10

@@20:
    push cx
    jcxz @@30
    cld
    rep movsb

@@30:
    push es     ;Push es onto stack
    pop ds      ;Make ds = es
    pop cx      ;Restore length to cx
    xor bx, bx  ;Initialize parameter count
    jcxz @@60   ;Skip loop if length = 0
    mov Si, offset params

@@40:
    call Separators         ;Check for blank, tab, or cr
    jne @@50                ;Jump if not a separator
    mov [byte ptr si], 0    ;Change separator to null
    inc bx                  ;Count number of parameters

@@50:  
    inc si
    loop @@40

@@60: 
    mov [numParams],bx
    ret
ENDP GetParams

PROC GetOneParam
    xor al, al ;Init search value
    mov di, offset params 
    jcxz @@99   ;If number=0, jump to exit
    cmp cx, [numParams] ;Compare number with max
    jae @@99 ;Exit if > maximum number
    cld 
@@10:
    scasb
    jnz @@10
    loop @@10
@@99:
    inc cx 
    ret
ENDP GetOneParam
 
PROC StrLength
    push ax
    push di

    xor al,al
    mov cx,0ffffh
    cld
    repnz scasb
    not cx
    dec cx
    
    pop di
    pop ax
    ret 
ENDP StrLength









PROC StrWrite
    call StrLength
PROC StrWrite2
    push ax
    push bx
    push dx

    mov bx,1
    mov dx,di
    mov ah, 40h
    int 21h
    pop dx
    pop bx
    pop ax
ENDP StrWrite2
    ret
ENDP StrWrite

PROC    HexDigit
        cmp     dl, 10          ; Is dl < 10 (i.e. hex 'A')?
        jb      @@10            ; If yes, jump
        add     dl, 'A'-10      ; Else convert to A, B, C, D, E, or F
        ret                     ; Return to caller
@@10:
        or      dl, '0'         ; Convert digits 0 to 9
        ret                     ; Return to caller
ENDP    HexDigit

PROC    NumToASCII              ; Normal entry point
        push    dx              ; Save some modified registers
        push    di
        push    si

; si = count of digits on stack

        xor     si, si          ; Set digit-count to zero
        jcxz    @@20            ; If cx=0, jump to set cx=1
@@10:
        xor     dx, dx          ; Extend ax to 32-bit dxax
        div     bx              ; ax<-axdx div bx; dx<-remainder
        call    HexDigit        ; Convert dl to ASCII digit
        push    dx              ; Save digit on stack
        inc     si              ; Count digits on stack
        loop    @@10            ; Loop on minimum digit count
@@20:
        inc     cx              ; Set cx = 1 in case not done
        or      ax, ax          ; Is ax = 0? (all digits done)
        jnz     @@10            ; If ax <> 0, continue conversion
        mov     cx, si          ; Set cx to stack char count
        jcxz    @@40            ; Skip next loop if cx=0000
        cld                     ; Auto-increment di for stosb
@@30:
        pop     ax              ; Pop next digit into al
        stosb                   ; Store digit in string; advance di
        loop    @@30            ; Loop for cx digits
@@40:
        mov     [byte di], ASCnull      ; Store null at end of string
        pop     si              ; Restore saved registers
        pop     di
        pop     dx

        ret                     ; Return to caller
ENDP    NumToASCII

PROC    BinToAscDec
        push    bx              ; Save bx on stack
        mov     bx, 10          ; Set base = 10 (decimal)
        call    NumToAscii      ; Convert ax to ASCII
        pop     bx              ; Restore bx
        ret                     ; Return to caller
ENDP    BinToAscDec

PROC    StrCompare
        push    ax              ; Save modified registers
        push    di
        push    si
        cld                     ; Auto-increment si
@@10:
        lodsb                   ; al <- [si], si <- si + 1
        scasb                   ; Compare al and [di]; di <- di + 1
        jne     @@20            ; Exit if non-equal chars found
        or      al, al          ; Is al=0? (i.e. at end of s1)
        jne     @@10            ; If no jump, else exit
@@20:
        pop     si              ; Restore registers
        pop     di
        pop     ax
        ret                     ; Return flags to caller
ENDP    StrCompare
;---------------------------------------------------------------
; (3Dh) DOS_OpenFile    Open file for I/O
;---------------------------------------------------------------
; Input:
;       fileName = label of ASCIIZ string in ds data segment
; Output:
;       cf = 0 : (jnc) File opened
;       ax = file handle for future operations
;
;       cf = 1 : (jc) File not opened
;       ax = error code
;            2 = file not found
;            3 = path not found
;            4 = no more handles available
;            5 = access denied
; Registers:
;       ax, dx
;---------------------------------------------------------------
PROC   DOS_OpenFile  
        mov     dx,  di  ;; Assign name address to ds:dx
        xor     al, al          ;; Open for read/write access
        mov     ah, 3Dh         ;; Assign DOS function number
        int     21h             ;; Call DOS
        ret
ENDP    DOS_OpenFile

;---------------------------------------------------------------
; (3Eh) DOS_CloseFile   Close a previously opened file
;---------------------------------------------------------------
; Input:
;       bx = file handle from DOS_CreateFile or DOS_OpenFile
; Output:
;       cf = 0 : (jnc) File closed
;
;       cf = 1 : (jc) File not closed
;       ax = error code
;            6 = bad handle or file was not open
; Registers:
;       ax
;---------------------------------------------------------------
MACRO   DOS_CloseFile
        mov     ah, 3Eh         ;; Assign DOS function number
        int     21h             ;; Call DOS
ENDM    DOS_CloseFile

;---------------------------------------------------------------
; (3Fh) DOS_ReadFile    Read from file or device
;---------------------------------------------------------------
; Input:
;       bx = file handle from DOS_CreateFile or DOS_OpenFile
;       cx = number of bytes requested to read
;       buffer = label of destination buffer in ds data segment
;       Note: buffer must be at least cx bytes long!
; Output:
;       cf = 0 : (jnc) Read was successful
;       ax = actual number of bytes read (0=at end of file)
;
;       cf = 1 : (jc) Read was not successful
;       ax = error code
;            5 = access denied
;            6 = bad handle or file was not open
; Registers:
;       ax, dx
;---------------------------------------------------------------
MACRO   DOS_ReadFile  buffer
        mov     ah, 40h         ;; Assign DOS function number
        mov     dx, OFFSET buffer  ;; Address buffer with ds:dx
        int     21h             ;; Call DOS
ENDM    DOS_ReadFile


Start:
    mov ax, @data
    mov es,ax 
    call GetParams
    call ParamCount

    mov ax, 0
    cmp dx, ax  ; if zero params
    je Invalid


    call GetOneParam
    mov si, offset helpstr
    call StrCompare
    je Help 


    mov si, offset filArg
    call StrCompare
    je ProcessFile


    mov di, offset s1  
    call StrWrite
    mov ax, dx 
    mov cx, 1
    mov di, offset string
    call BinToAscDec
    call StrWrite
    jmp koniec

ProcessFile:
    ;mov di, offset stra
    call GetOneParam


    mov dx, di
    xor al,al
    mov ah, 3Dh
    int 21h
    mov [inputFile],ax


FirstreadFile:
    mov ah, 3Fh
    mov bx, [inputFile]
    mov cx, 10;8191
    mov dx, offset buff
    int 21h
    jmp ProcessBuffer


readFile:
    mov di,offset buff
    call StrNull
    mov dx,di
    xor si, si
    mov ah, 3Fh
    mov bx, [inputFile]
    mov cx, 10;8191
    mov dx, offset buff
    mov si, dx
    int 21h
    jmp checkstart

ProcessBuffer:
    or      ax, ax
    jz koniec
    xor bx,bx
    inc bx
    mov [lineCount],bx
    push ax
    push di
    push cx
    mov ax, [lineCount]
    mov di, offset string
    mov cx, 1
    call BinToAscDec
    call StrWrite
    mov di, offset Space
    call StrWrite
    pop cx
    pop di
    pop ax

    mov si,dx

checkstart:
    or      ax, ax
    jz koniec

Comp:    
    cmp [byte si], lf 
    je PRocLF  ; end of line
    cmp [byte si],ASCnull ;eof
    je check

    push si
    push dx
    dec dx
    sub si, dx  ;end of buffer
    cmp si, ax 
    mov cx, ax
    pop dx
    pop si
    je Printnl

    add si,1
    jne Comp

PRocLF:
    ;print
    push bx
    mov bx, [lineCount]
    inc bx
    mov [lineCount],bx
    pop bx

    inc si
    push si
    sub si, dx 
    mov cx, si ;; num of chars
    
    mov di, dx ; assigning buffer 
    call StrWrite2
    sub ax, cx
    pop si

    mov dx, si
    push ax
    push di
    push cx
    mov ax, [lineCount]
    mov di, offset string
    mov cx, 1
    call BinToAscDec
    call StrWrite
    mov di, offset Space
    call StrWrite
    pop cx
    pop di
    pop ax
    jmp Comp

check:
    or      ax, ax
    jz koniec
    jnz readFile

Print:
    push cx 
    push si
    mov di, dx 
    call StrWrite2
    pop si
    pop cx
    add si,1
    jmp Comp

Printnl:
    push cx 
    push si
    mov di, dx 
    call StrWrite2
    pop si
    pop cx
    add si,1
    jmp readFile

Invalid:
    mov di, offset InvalidArgs
    call StrWrite
    jmp koniec

Help:
    mov di, offset HelpString
    call StrWrite
    jmp koniec

koniec:
    mov ah, 04Ch
    int 21h
END Start
END