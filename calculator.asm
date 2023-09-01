PUTC    MACRO   char
        PUSH    AX
        MOV     AL, char
        MOV     AH, 0Eh
        INT     10h     
        POP     AX
ENDM 


display_menu:
    push ax
    
    ; Display menu options
    mov ah, 0Ch ; BIOS function to set color and print character
    mov al, '1' ; Option 1
    mov bh, 0 ; Black background
    mov bl, 15 ; White foreground
    int 10h
    mov al, ')'
    int 10h
    mov al, ' '
    int 10h
    mov dx, offset option1_text
    int 21h
    
    mov al, 13 ; Carriage return
    int 10h
    
    mov al, '2' ; Option 2
    int 10h
    mov al, ')'
    int 10h
    mov al, ' '
    int 10h
    mov dx, offset option2_text
    int 21h
    
    mov al, 13 ; Carriage return
    int 10h
    
    mov al, '3' ; Option 3
    int 10h
    mov al, ')'
    int 10h
    mov al, ' '
    int 10h
    mov dx, offset option3_text
    int 21h
    
    mov al, 13 ; Carriage return
    int 10h
    
    mov al, '4' ; Option 4
    int 10h
    mov al, ')'
    int 10h
    mov al, ' '
    int 10h
    mov dx, offset option4_text
    int 21h
    
    mov al, 13 ; Carriage return
    int 10h
    
    mov al, '5' ; Option 5
    int 10h
    mov al, ')'
    int 10h
    mov al, ' '
    int 10h
    mov dx, offset option5_text
    int 21h
    
    ; Wait for user input
    mov ah, 01h ; BIOS function to read keyboard input
    int 21h
    
    pop ax
    ret
    
option1_text db 'Option 1', 0
option2_text db 'Option 2', 0
option3_text db 'Option 3', 0
option4_text db 'Option 4', 0
option5_text db 'Option 5', 0







data segment 
     chain_base1 db '----------ENTRER EN MAJUSCULE-----------  : $', 24h
     chain_base2 db '       D:------> DECIMAL $ ', 24h
     chain_base3 db '       H:------> HEXADECIMAL$', 24h
     chain_base4 db '       B:------> BINNAIRE $'  , 24h 
     chain_base5 db '       E:------> EXIT $'  , 24h 
     chain_base6 db '       I:------> INFO $'  , 24h
     
     chain_op1 db 'Entrer le premier operande: $ ', 24h
     chain_op2 db 'Entrer le deuxieme operande: $', 24h
     chain_op11 db '----------Choisir l''operation-----------$', 24h
     chain_op22 db '          + ---------> : addition $' 
     chain_op33 db '          - ---------> : soustraction $'
     chain_op44 db '          / ---------> : division $'
     chain_op55 db '          * ---------> : multiplication $'
     
     chain_erreur db 'division par 0  !!!!!!!!!!$', 24h 
     wlcm dw '************wlcm our calculator developed by kheira************ $' 
     key dw 'press any key to continue...$'
     keyy db 4 dup(?)
     exitt db 'this calculator developed bykheira $ '
     exitt1 db 'groupe 4 $'
     exitt2 db 'monome ...$'
     exitt3 db 'thanks $' ,24h                  
     infoo db '-----our calculator trait the 3 case of bases :Binnary ,hexadecimal ,DEcimal $',24h
     infoo1 db 'it give u the ability of the 4 operations $'
     infoo2 db '++++ ading ,------sub , ****** mul ,/////// div $',24h
     infoo3 db '  thanks for usig our app $ ',24h
     chain_overflow db '    overflow !!!!    $',24h
     ;decimal variables
     Vald1 dw 0
     Vald2 dw 0
     
     ;binnaire variables
     
     Bin1 DB 19 DUP(0)     
     Bin2 DB 19 DUP(0) 
     Valb1 dw 0
     Valb2 dw 0 
     
     ;hexadecimal variables
     Hex1 DB 7 DUP(0)
     Hex2 DB 7 DUP(0) 
     Valh1 dw 0
     Valh2 dw 0 
     ;others
     
     base db 4 dup(?)
     op db 0 
     resultat dw 0
     signe dw 'le premier est inferieur au deuxieme'
     overflow_msg dw 'overflow'
ends 

stack segment 
    dw 128 dup(0)
ends 

code segment 
    start:
    mov ax,data 
    mov ds,ax
    mov es,ax 
    ; demmander a l'utilisateur de donner la base 
     debut:
    mov dx ,offset wlcm
    mov ah ,09h
    int 21h
    call sautdelignefunction
    call sautdelignefunction  
    mov dx ,offset key
    mov ah ,09h
    int 21h 
    mov keyy[0],2
    mov dx, offset keyy 
    mov ah , 0ah
    int 21h 
    call sautdelignefunction  
    
    mov dx ,offset chain_base1
    mov ah ,09h
    int 21h
 
    call sautdelignefunction
                           
    
    mov dx ,offset chain_base2
    mov ah ,09h
    int 21h 
    call sautdelignefunction
    
    mov dx ,offset chain_base3
    mov ah ,09h
    int 21h
    
    call sautdelignefunction
    
    mov dx ,offset chain_base4
    mov ah ,09h
    int 21h
   
    call sautdelignefunction
    
    mov dx ,offset chain_base5
    mov ah ,09h
    int 21h
   
    call sautdelignefunction
    
    mov dx ,offset chain_base6
    mov ah ,09h
    int 21h  
    call sautdelignefunction
    mov base[0],2   
    mov dx, offset base 
    mov ah , 0ah
    int 21h 
    call sautdelignefunction
          
    decimal:
    mov si,offset base
    cmp [si+2], 'D'
    jne binaire  
     ;entrer le 1er operande  
       mov dx ,offset chain_op1
       mov ah ,09h
       int 21h
       call SCAN_NUM;
       mov Vald1, cx ; 
       call sautdelignefunction
     ;entrer le 2eme operande 
        mov dx ,offset chain_op2
        mov ah ,09h
        int 21h 
        call SCAN_NUM;
        mov Vald2, cx ; 
        call sautdelignefunction
      ; demander a l'utilisateur de donner l'operation  
    bcl: 
    mov dx ,offset chain_op11
    mov ah ,09h
    int 21h
    call sautdelignefunction
    
    mov dx ,offset chain_op22
    mov ah ,09h
    int 21h
    call sautdelignefunction
    
     mov dx ,offset chain_op33
    mov ah ,09h
    int 21h
    call sautdelignefunction
    
    mov dx ,offset chain_op44
    mov ah ,09h
    int 21h
    call sautdelignefunction
    
     mov dx ,offset chain_op55
    mov ah ,09h
    int 21h 
    mov ah , 01h
    int 21h
    call sautdelignefunction 
    mov [op] , al
    call sautdelignefunction
      
    ; effectuer les operations  
        
     
     cmp [op], '+'
      jne Sous  
        mov ax, Vald1;
        add ax, Vald2;
        jc overflow
        jmp finD
 Sous:
     cmp [op] , '-'
     jne Mult   
     mov ax, Vald1 
      cmp ax , Vald2 
       jl errors 
          sub ax, Vald2
           jc overflow 
          jmp finD 
        errors: 
         mov dx ,offset chain_erreur
         mov ah ,09h
         int 21h 
         jmp fin;
        
         
 Mult:
      cmp [op] , '*'
      jne Divis 
        mov cx , Vald2
        mov ax , Vald1 
        Mul cx ;
         jc overflow 
        jmp finD
 Divis:  
        cmp [op], '/'
        jne bcl
        mov ax, Vald1
        mov cx, Vald2
        cmp cx, 0
        je errord
        xor dx, dx ; clear dx before dividing
        div cx 
         jc overflow
        jmp finD

       errord: 
         mov dx ,offset chain_erreur
         mov ah ,09h
         int 21h 
         jmp fin
         
       
      find: 
        push Vald1 ; 
        push Vald2 ; 
        push ax 
        call afficherValeur
        add sp,6
        
         
     call sautdelignefunction  
      
    jmp debut  
    
    
    binaire:
    
    cmp [si+2], 'B'
    jne hexadecimal
    
    ;entrer le 1er operande
    relireB1:
    mov dx ,offset chain_op1
    mov ah ,09h
    int 21h
    mov Bin1[0],17   
    mov dx, offset Bin1 
    mov ah , 0ah
    int 21h 
    call sautdelignefunction
    ;verifier B1 est binaire
    push offset Bin1
    call estBinaire
    cmp al,0
    je relireB1  
    push offset Bin1
    call StringToBinary
    mov Valb1 , ax 
    ;entrer le 2eme operande 
    relireB2:
    mov dx ,offset chain_op2
    mov ah ,09h
    int 21h
    mov Bin2[0],17   
    mov dx, offset Bin2 
    mov ah , 0ah
    int 21h 
    call sautdelignefunction
    ;verifier B2 est binaire
    push offset Bin2
    call estBinaire
    cmp al,0
    je relireB2  
    push offset Bin2
    call StringToBinary
    mov Valb2 , ax   
     ; demander a l'utilisateur de donner l'operation  
    bcl1: 
     mov dx ,offset chain_op11
    mov ah ,09h
    int 21h
    call sautdelignefunction
    
    mov dx ,offset chain_op22
    mov ah ,09h
    int 21h
    call sautdelignefunction
    
     mov dx ,offset chain_op33
    mov ah ,09h
    int 21h
    call sautdelignefunction
    
    mov dx ,offset chain_op44
    mov ah ,09h
    int 21h
    call sautdelignefunction
    
     mov dx ,offset chain_op55
    mov ah ,09h
    int 21h 
    mov ah , 01h
    int 21h
    call sautdelignefunction 
    mov [op] , al
    call sautdelignefunction
    
    
     cmp [op], '+'
      jne SB  
        mov ax, Valb1;
        add ax, Valb2;
         jc overflow
        jmp finb
 SB:
     cmp [op] , '-'
     jne MB   
     mov ax, Valb1 
      cmp ax , Valb2 
       jl errorsb 
          sub ax, Valb2
           jc overflow
          
          jmp finb 
        errorsb: 
         mov dx ,offset chain_erreur
         mov ah ,09h
         int 21h 
         jmp fin;
         
        
         
 MB:
      cmp [op] , '*'
      jne DBB 
        mov cx , Valb2
        mov ax , Valb1 
        Mul cx ;
         jc overflow 
        jmp finb
 DBB:  
    cmp [op], '/'
        jne bcl1
        mov ax, Valb1
        mov cx, Valb2
        cmp cx, 0
        je errordb
        xor dx, dx ; clear dx before dividing
        div cx   
         jc overflow
        jmp finb
       errordb: 
         mov dx ,offset chain_erreur
         mov ah ,09h
         int 21h 
         jmp fin;
      finb:
        mov resultat,ax 
        push Valb1 ; 
        push Valb2 ; 
        push resultat 
        call afficherValeurb
        add sp,6 
       
    call sautdelignefunction 
    jmp debut   
    
    hexadecimal: 
    
    cmp [si+2], 'H'
    jne exit
    ;entrer le 1er operande
    relireH1:
    mov dx ,offset chain_op1
    mov ah ,09h
    int 21h
    mov Hex1[0],5   
    mov dx, offset Hex1 
    mov ah , 0ah
    int 21h 
    call sautdelignefunction 
    mov valh1,ax
    ;verifier H1 est hexadecimal
    push offset Hex1
    call estHexa
    cmp al,0
    je relireH1  
    push offset Hex2
    call StringToHex
    mov valh1,ax
    ;entrer le 2eme operande 
    relireH2:
    mov dx ,offset chain_op2
    mov ah ,09h
    int 21h
    mov Hex2[0],5   
    mov dx, offset Hex2 
    mov ah , 0ah
    int 21h 
    call sautdelignefunction
    ;verifier H2 est hexadecimal
    push offset Hex2
    call estHexa
    cmp al,0
    je relireH2 
    push offset Hex2
    call StringToHex
    mov valh2,ax
     ; demander a l'utilisateur de donner l'operation  
    bcl2: 
    mov dx ,offset chain_op11
    mov ah ,09h
    int 21h
    call sautdelignefunction
    
    mov dx ,offset chain_op22
    mov ah ,09h
    int 21h
    call sautdelignefunction
    
     mov dx ,offset chain_op33
    mov ah ,09h
    int 21h
    call sautdelignefunction
    
    mov dx ,offset chain_op44
    mov ah ,09h
    int 21h
    call sautdelignefunction
    
     mov dx ,offset chain_op55
    mov ah ,09h
    int 21h 
    mov ah , 01h
    int 21h
    call sautdelignefunction 
    mov [op] , al
    call sautdelignefunction
    
    cmp [op], '+'
    jne SUBTRACT
    mov ax, Valh1
    add ax, Valh2
     jc overflow
    jmp DONE
    SUBTRACT:
    cmp [op], '-'
    jne MULTIPLY
    mov ax, Valh1
    sub ax, Valh2
     jc overflow
    jmp DONE
    MULTIPLY:  
    cmp [op], '*'
    jne DIVIDE
    mov bx, Valh1
    mul Valh2 
     jc overflow
    mov ax, bx
    jmp DONE
    DIVIDE:
    cmp [op], '/'
    jne bcl2
    mov cx, Valh2
    mov ax, Valh1
    cmp cx, 0
    je ERROR
    xor dx, dx
    div cx 
     jc overflow
    ;mov ax, bx
    jmp DONE
    ERROR:
    mov dx, offset chain_erreur
    mov ah, 09h
    int 21h
    jmp DONE
    DONE:
    mov resultat,ax
    push Valh1
    call afficherValeurh
    add sp,2
    mov ah,02h
    mov dl,[op]
    int 21h
    
    
    
    push valh2
    call afficherValeurh
    add sp,2
    mov ah,02h
    mov dl,[op]
    int 21h
    mov ah,02h
    mov dl,'='
    int 21h
    push resultat
    call afficherValeurh
    add sp,6 
    call sautdelignefunction
    jmp debut
    
exit:
    mov si,offset base
    cmp [si+2], 'E'
    jne info  
      
       mov dx ,offset exitt
       mov ah ,09h
       int 21h      
       call sautdelignefunction
       
       mov dx ,offset exitt1
       mov ah ,09h
       int 21h
       call sautdelignefunction
       
       mov dx ,offset exitt2
       mov ah ,09h
       int 21h
       call sautdelignefunction
       
       mov dx ,offset exitt3
       mov ah ,09h
       int 21h
       call sautdelignefunction
       jmp fin

info:
    mov si,offset base
    cmp [si+2], 'I'
    jne debut  
      
       mov dx ,offset infoo
       mov ah ,09h
       int 21h      
       call sautdelignefunction
       
       mov dx ,offset infoo1
       mov ah ,09h
       int 21h
       call sautdelignefunction
       
       mov dx ,offset infoo2
       mov ah ,09h
       int 21h
       call sautdelignefunction
       
       mov dx ,offset infoo3
       mov ah ,09h
       int 21h
       call sautdelignefunction
       jmp debut
 
overflow:
    mov dx ,offset chain_overflow
    mov ah ,09h
    int 21h
fin:

    
      
mov ax, 4ch
int 21h  

StringToBinary PROC


; Traiter la chaine se trouvant à @=DX à partir de indice SI=2 à [DX+1]+1 (dernier caractère à indice BString[1]+1)
MOV AX, 0
MOV BX, DX
MOV CX, 0 
MOV CL, [BX+1];=la taille reel de la chaine
ADD CX, 1  ;position du fin
MOV SI, 1
; Exemple du résultat : chaine lue = 0101 alors AX = 0000 0000 0000 0101

Reboucler2:
INC SI
CMP SI, CX
JG Fin3
SHL AX, 1 ; // Décalage à gauche avec insertion de 0 de la droite
CMP [BX+SI], '1'

JNE Reboucler2 ; Si BString[SI]=0 alors le 0 est déjà inséré par SHL
ADD AX, 1 ; Si BString[SI]=1 alors remplacer le 0 déjà inséré par 1 en faisant add de 1
JMP Reboucler2
Fin3:
RET 
StringToBinary ENDP 

 StringToHex PROC
    MOV AX, 0
    MOV BX, DX
    MOV CX, 0
    MOV CL, [BX+1] ; Set the length of the string in CL
    ADD CX, 1      ; Set the position of the end of the string
    MOV SI, 1

Reboucler:
    INC SI
    CMP SI, CX
    JG Finhex
    SHL AX, 4      ; Shift left by 4 bits (equivalent to multiplying by 16)
    MOV BL, [BX+SI]
    CMP BL, '0'
    JB InvalidChar ; If the character is not between '0' and '9' or 'A' to 'F', exit with an error.
    SUB BL, '0'
    CMP BL, 10
    JAE CheckUpper
    ADD AL, BL     ; Add the numeric value of the character to the low byte of AX
    JMP Reboucler
CheckUpper:
    SUB BL, 7      ; If the character is between 'A' and 'F', subtract 7 to get the numeric value
    ADD AL, BL     ; Add the numeric value of the character to the low byte of AX
    JMP Reboucler
InvalidChar:
    ;MOV AX, -1     ; Return -1 to indicate an invalid character was found
    RET
Finhex:
    RET
StringToHex ENDP

    estBinaire PROC
       push bx
       push cx
       push si
       push bp
       mov bp,sp
       mov al,1
       mov si,1
       mov cx,0
       mov bx,[bp+10] 
       mov cl,[bx+si]
       add cx,1
       rebouclerb:
       inc si  
       cmp si,cx
       jg finbb
       cmp [bx+si],'0'
       je rebouclerb
       cmp [bx+si],'1'
       je rebouclerb
    
       mov al,0
       finbb:
       pop bp
       pop si
       pop cx
       pop bx
       ret
    estBinaire endp
    
    estHexa PROC
       push bx
       push cx
       push si
       push bp
       mov bp,sp
       mov al,1
       mov si,1
       mov cx,0
       mov bx,[bp+10] 
       mov cl,[bx+si]
       add cx,1
       rebouclerh:
       inc si  
       cmp si,cx
       jg finh
       cmp [bx+si],'0'
       jl nothexa
       cmp [bx+si],'F'
       jg nothexa
       
       jmp rebouclerh
       nothexa:
       mov al,0
       finh:
       pop bp
       pop si
       pop cx
       pop bx
       ret
    estHexa endp

 
    sautdelignefunction proc
        push dx
        push ax
        mov ah,02h
        mov dl,0ah
        int 21h 
        mov ah,02h
        mov dl,0dh
        int 21h
        pop ax
        pop dx
        ret
    sautdelignefunction endp 
    afficherValeur PROC
          mov bp, sp 
          mov ax , [bp+6]
          mov cx,0
          mov dx,0
          mov bx,10
          
          empiler1:
          div bx     ;res=ax; diviser ax par bx ; rest=dx
          add dx,48  ;ajouter au reste de la division 48 pour convertir le nb en dicimal
          push dx     ; empiler dx
          mov dx,0    ; rendre dx a 0
          inc cx       ; inc cx "cobient d'iter; loop works with cx
          cmp ax,0
          jne empiler1
          
    
         
          depiler1:
          pop dx       ; depiler dans dx
          mov ah,02h
          int 21h
          loop depiler1 
          
    
    mov ah ,02h
    mov dl , [op]
    int 21h
          
          mov ax , [bp+4]
          mov cx,0
          mov dx,0
          mov bx,10  
          empiler2:
          div bx     ;res=ax; diviser ax par bx ; rest=dx
          add dx,48  ;ajouter au reste de la division 48 pour convertir le nb en dicimal
          push dx     ; empiler dx
          mov dx,0    ; rendre dx a 0
          inc cx       ; inc cx "cobient d'iter; loop works with cx
          cmp ax,0
          jne empiler2
          
         
         
          depiler2:
          pop dx       ; depiler dans dx
          mov ah,02h
          int 21h
          loop depiler2 
                         
                         
                         
                         
        mov ah, 02h ; fonction d'affichage de caractere
        mov dl, '='
        int 21h
          
          mov ax , [bp+2]
          mov cx,0
          mov dx,0
          mov bx,10  
          empiler3:
          div bx     ;res=ax; diviser ax par bx ; rest=dx
          add dx,48  ;ajouter au reste de la division 48 pour convertir le nb en dicimal
          push dx     ; empiler dx
          mov dx,0    ; rendre dx a 0
          inc cx       ; inc cx "cobient d'iter; loop works with cx
          cmp ax,0
          jne empiler3
          
         
         
          depiler3:
          pop dx       ; depiler dans dx
          mov ah,02h
          int 21h
          loop depiler3 
          
    RET 

afficherValeur endp
    
 SCAN_NUM        PROC    NEAR
        PUSH    DX
        PUSH    AX
        PUSH    SI
        
        MOV     CX, 0

        ; reset flag:
        MOV     CS:make_minus, 0

next_digit:

        ; get char from keyboard
        ; into AL:
        MOV     AH, 00h
        INT     16h
        ; and print it:
        MOV     AH, 0Eh
        INT     10h

        ; check for MINUS:
        CMP     AL, '-'
        JE      set_minus

        ; check for ENTER key:
        CMP     AL, 13  ; carriage return?
        JNE     not_cr
        JMP     stop_input
not_cr:


        CMP     AL, 8                   ; 'BACKSPACE' pressed?
        JNE     backspace_checked
        MOV     DX, 0                   ; remove last digit by
        MOV     AX, CX                  ; division:
        DIV     CS:ten                  ; AX = DX:AX / 10 (DX-rem).
        MOV     CX, AX
        PUTC    ' '                     ; clear position.
        PUTC    8                       ; backspace again.
        JMP     next_digit
backspace_checked:


        ; allow only digits:
        CMP     AL, '0'
        JAE     ok_AE_0
        JMP     remove_not_digit
ok_AE_0:        
        CMP     AL, '9'
        JBE     ok_digit
remove_not_digit:       
        PUTC    8       ; backspace.
        PUTC    ' '     ; clear last entered not digit.
        PUTC    8       ; backspace again.        
        JMP     next_digit ; wait for next input.       
ok_digit:


        ; multiply CX by 10 (first time the result is zero)
        PUSH    AX
        MOV     AX, CX
        MUL     CS:ten                  ; DX:AX = AX*10
        MOV     CX, AX
        POP     AX

        ; check if the number is too big
        ; (result should be 16 bits)
        CMP     DX, 0
        JNE     too_big

        ; convert from ASCII code:
        SUB     AL, 30h

        ; add AL to CX:
        MOV     AH, 0
        MOV     DX, CX      ; backup, in case the result will be too big.
        ADD     CX, AX
        JC      too_big2    ; jump if the number is too big.

        JMP     next_digit

set_minus:
        MOV     CS:make_minus, 1
        JMP     next_digit

too_big2:
        MOV     CX, DX      ; restore the backuped value before add.
        MOV     DX, 0       ; DX was zero before backup!
too_big:
        MOV     AX, CX
        DIV     CS:ten  ; reverse last DX:AX = AX*10, make AX = DX:AX / 10
        MOV     CX, AX
        PUTC    8       ; backspace.
        PUTC    ' '     ; clear last entered digit.
        PUTC    8       ; backspace again.        
        JMP     next_digit ; wait for Enter/Backspace.
        
        
stop_input:
        ; check flag:
        CMP     CS:make_minus, 0
        JE      not_minus
        NEG     CX
not_minus:

        POP     SI
        POP     AX
        POP     DX
        RET
make_minus      DB      ?       ; used as a flag.
ten             DW      10      ; used as multiplier.
SCAN_NUM        ENDP  

BIN_TO_DEC PROC
    PUSH BP      ; Sauvegarde la base d'empilement actuelle
    MOV BP, SP   ; Initialise la base d'empilement avec le pointeur de pile actuel

    MOV BX, 10   ; Initialise BX avec la valeur 10 (la base décimale)
    XOR CX, CX   ; Initialise CX à 0 (le compteur de puissance de 2)
    XOR DX, DX   ; Initialise DX à 0 (le chiffre binaire courant)
    
    ; Boucle pour calculer la valeur décimale
    LOOP:
        SHL AX, 1     ; Décale à gauche le registre AX pour obtenir le prochain chiffre binaire
        JC ADD_ONE    ; Si le bit de poids fort de AX est 1, alors passe à l'étiquette ADD_ONE
        JMP CONTINUE  ; Sinon, passe à l'étiquette CONTINUE

    ADD_ONE:
        ADD DX, 1     ; Ajoute 1 au chiffre binaire courant (DX)
        SUB AX, 1     ; Soustrait 1 de AX pour mettre le bit de poids fort à 0

    CONTINUE:
        ADD CX, 1     ; Incrémente le compteur de puissance de 2
        MUL BX       ; Multiplie BX par 10 pour obtenir la nouvelle puissance de 10
        ADD AX, DX   ; Ajoute le chiffre binaire courant à AX
        CMP AX, 0    ; Vérifie si AX est égal à 0
        JNE LOOP     ; Si AX est différent de 0, passe à l'étiquette LOOP

    POP BP        ; Restaure la base d'empilement précédente
    RET          ; Retourne la valeur décimale dans AX
BIN_TO_DEC ENDP
                
; Convert a hex value in AX to a decimal value
HexToDec PROC
PUSH BP ; Sauvegarde la base d'empilement actuelle
MOV BP, SP ; Initialise la base d'empilement avec le pointeur de pile actue
XOR CX, CX   ; Initialise CX à 0 (le compteur de puissance de 16)
XOR DX, DX   ; Initialise DX à 0 (le chiffre hexadécimal courant)

; Boucle pour calculer la valeur décimale
LOOPh:
    XOR BX, BX    ; Initialise BX à 0 pour récupérer le chiffre hexadécimal courant
    MOV BL, AL    ; Déplace le contenu de AX dans BL pour extraire le chiffre hexadécimal courant
    AND BL, 0Fh   ; Masque les bits supérieurs pour ne conserver que les 4 bits de poids faible
    ADD AX, AX    ; Décale à gauche le registre AX pour obtenir le prochain chiffre hexadécimal
    CMP BL, 0Ah   ; Vérifie si le chiffre hexadécimal courant est supérieur ou égal à 10
    JL ADD_NUM    ; Si le chiffre hexadécimal courant est inférieur à 10, passe à l'étiquette ADD_NUM
    SUB BL, 7     ; Si le chiffre hexadécimal courant est supérieur ou égal à 10, soustrait 7 pour obtenir le chiffre décimal correspondant
    ADD DX, BX    ; Ajoute le chiffre décimal courant à DX
    JMP CONTINUEh  ; Passe à l'étiquette CONTINUE

ADD_NUM:
    ADD DX, BX    ; Ajoute le chiffre hexadécimal courant à DX

CONTINUEh:
    ADD CX, 1     ; Incrémente le compteur de puissance de 16
    MUL CX        ; Multiplie CX par 16 pour obtenir la nouvelle puissance de 16
    CMP AX, 0    ; Vérifie si AX est égal à 0
    JNE LOOPh     ; Si AX est différent de 0, passe à l'étiquette LOOP

POP BP        ; Restaure la base d'empilement précédente
ret 
HexToDec endp







afficherValeurb PROC 
        mov bp ,  sp
        mov bx,[bp+6]
        mov cx,16 
        
        print:
         
        mov ah,02h  ;num service
        mov dl ,'0'
        test bx,1000000000000000b;et logic between all bits, 11 ou 10
        jz zero
        mov dl,'1'  

        zero:
        int 21h
        shl bx,1
        loop print 
        
                  
            
            mov ah ,02h
            mov dl , [op]
            int 21h
                  
        mov bx,[bp+4]
        mov cx,16 
        
        print1:
         
        mov ah,02h  ;num service
        mov dl ,'0'
        test bx,1000000000000000b;et logic between all bits, 11 ou 10
        jz zero1
        mov dl,'1'  
        
        zero1:
        int 21h
        shl bx,1
        loop print1 
        
         mov ah ,02h
            mov dl , '='
            int 21h
        mov     bx,[bp+2]
        mov     cx,16 
        
        print2:
         
        mov ah,02h  ;num service
        mov dl ,'0'
        test bx,1000000000000000b;et logic between all bits, 11 ou 10
        jz zero2
        mov dl,'1'  
        
        zero2:
        int 21h
        shl bx,1
        loop print2 
        
        RET

afficherValeurb endp

afficherValeurh proc

; Push BP onto the stack and move SP into BP
push bp
mov bp, sp

; Move the word at [BP+4] (second argument) into BX
mov bx, [bp+4]

; Extract the first hex digit and display it
mov ax, bx
shr ax, 12
and ax, 000Fh
cmp ax, 9
jle afficherChiffre
add al, 37h ; Convert to hex
jmp afficherCaractere

afficherChiffre:
add al, 30h ; Convert to ASCII

afficherCaractere:
mov dl, al
mov ah, 02h ; Display character
int 21h

; Extract the second hex digit and display it
mov ax, bx
shr ax, 8
and ax, 000Fh
cmp ax, 9
jle afficherChiffre2
add al, 37h ; Convert to hex
jmp afficherCaractere2

afficherChiffre2:
add al, 30h ; Convert to ASCII

afficherCaractere2:
mov dl, al
mov ah, 02h ; Display character
int 21h

; Extract the third hex digit and display it
mov ax, bx
shr ax, 4
and ax, 000Fh
cmp ax, 9
jle afficherChiffre3
add al, 37h ; Convert to hex
jmp afficherCaractere3

afficherChiffre3:
add al, 30h ; Convert to ASCII

afficherCaractere3:
mov dl, al
mov ah, 02h ; Display character
int 21h

; Extract the fourth hex digit and display it
mov ax, bx
and ax, 000Fh
cmp ax, 9
jle afficherChiffre4
add al, 37h ; Convert to hex
jmp afficherCaractere4

afficherChiffre4:
add al, 30h ; Convert to ASCII

afficherCaractere4:
mov dl, al
mov ah, 02h ; Display character
int 21h

; Pop BP from the stack and return from the procedure
pop bp
ret

               
ends
end start 