    IDEAL
    MODEL small
    CODESEG
    PUBLIC StrLength

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
ENDP StrWrite
   END
