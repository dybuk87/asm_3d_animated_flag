.model small
.386p
STACK_SIZE equ 1000h

d equ dword ptr 

; wymiar flagi (x,y)
resX equ 16
resY equ 16

Cell   equ 6
flagaX equ (resX/2)
flagaY equ (resY/2)

SHIFT_ equ 4

; struktura wierzcholkow(16 bajtow)
@r_x  equ 0
@r_y  equ 4
@r_z  equ 8
@r_dx equ 12
@r_dy equ 14

; struktuta kolorów(6bajtów + 2bajty dope³nienia = 8 bajtów)
@r_c1 equ 0
@r_c2 equ 2
@r_c3 equ 4

size_p equ (resX+1)*(resY+1)*16
faces_ equ resX*ResY*2


code16 segment para public use16
 assume cs:code16, ds:code16, ss:MY_STACK
start16:
 mov ax, code16
 mov ds, ax
 mov ax, my_stack
 mov ss, ax
 mov sp, stack_size
 sub sp, 2

 mov ax,0013h
 int 10h 

 lea  di, matrix
 call czysc_m
 
 call pall
 call nowa_flaga
 finit

@pet:
 lea  si, matrix
 call rot_yx

 mov ax, 09000h
 mov es, ax

 xor di, di
 mov ecx, 16000
 xor eax, eax
 rep stosd
 
 call flaga_to_2d
 fld a
 fadd adda
 fstp a

 fld ry
 fadd addry
 fstp ry

 lea di, faces
 lea si, point
 mov ecx, faces_
 mov ax, 3
@pet_fa: 
 push ax
 push ecx
 push di
 push si

 cmp ax, 1
 ja @kol1
  push word ptr 100
  push word ptr 100
  push word ptr 100
  jmp ok_kol
 @kol1:
  push word ptr 60
  push word ptr 60
  push word ptr 60  
 ok_kol:

 mov ax, [di]       ; w1
 mov bx, si
 add bx, ax         ; points[w1]
 push word ptr [bx+@r_dx]
 push word ptr [bx+@r_dy]

 mov ax, [di+2]     ; w2
 mov bx, si
 add bx, ax         ; points[w2]
 push word ptr [bx+@r_dx]
 push word ptr [bx+@r_dy]

 mov ax, [di+4]       ; w3
 mov bx, si
 add bx, ax         ; points[w1]
 push word ptr [bx+@r_dx]
 push word ptr [bx+@r_dy]
 call trian_g

 pop si
 pop di
 pop ecx
 pop ax
 add di,6

 and ax, ax
 jnz @ok_ax
  mov ax, 4
 @ok_ax:
  dec ax

loop @pet_fa

 call wait_f

 mov ax, 0A000h
 mov es, ax
 push ds
  mov ax, 09000h
  mov ds, ax
  xor si, si
  xor di, di
  mov cx, 16000
  rep movsd
 pop ds

 mov ah, 1
 int 16h
jz @pet
 
 mov ax, 0003h
 int 10h
 
 mov ah, 4ch
 int 21h

plot:
 shl ax, 6
 add di, ax
 shl ax, 2
 add di, ax
 mov es:[di], cl
ret

tmp DD 0

wait_f:
 push dx
 push ax
 mov dx,3dah
 trw:
 in al,dx
 test al,00001000b
 jnz trw

 ntrw:
 in al,dx
 test al,00001000b
 jz ntrw
 pop ax
 pop dx
ret

@x1  equ word ptr [bp+10]
@y   equ word ptr [bp+ 8]
@x2  equ word ptr [bp+ 6]
@c   equ word ptr [bp+ 4]
hline proc
 push bp
 mov  bp, sp
 mov  di, @x1
 mov  cx, @x2
 mov  ax, @y
 cmp  cx, di     
 jg @ok_        
  xchg cx, di
 @ok_:
 cmp  cx, 0
 jl @bye_
 cmp  di, 319
 jg @bye_
 cmp cx, 319
 jle @ok_2
  mov cx, 319
 @ok_2:
 cmp di, 0
 jge @ok_3
  xor di, di
 @ok_3:
  sub cx, di
  inc cx
  shl ax, 6
  add di, ax
  shl ax, 2
  add di, ax    
  mov ax, @c
  rep stosb  
 @bye_:
  pop bp  
 ret 8
endp

@x1_g   equ word ptr [bp+12]
@y_g    equ word ptr [bp+10]
@x2_g   equ word ptr [bp+ 8]
@c1_g   equ word ptr [bp+ 6]
@c2_g   equ word ptr [bp+ 4]
hline_g proc
 push 	bp
 mov  	bp, 	sp

 movsx	edi,	@x1_g
 movsx	ecx,	@x2_g
 cmp	edi,	ecx
 jle	@_ok_g
  xchg	ecx,	edi
  mov	ax,	@c1_g
  xchg	ax,	@c2_g
  mov	@c1_g,	ax
 @_ok_g:
 
 cmp	edi,	319
 jg	@_bye 
 
 push	ecx
 sub	ecx,	edi
 inc    ecx
 mov	ax,	@c2_g
 sub	ax,	@c1_g
 movsx	eax,	ax
 shl	eax,	16
 cdq
 idiv	ecx		
 mov	ebx,	eax	; ebx:=(c2-c1)*65536/(x2-x1)
 pop	ecx 

 movsx	eax,	@c1_g
 shl	eax,	16

 cmp	ecx,	320
 jl	@g_ok_hline_320
  mov 	ecx, 	320
 @g_ok_hline_320:

 cmp	edi,	0
 jge	@g_ok_hline_0
  neg	edi
  push	eax
  mov	eax,	ebx
  imul	edi
  pop	edi
  add	eax, 	edi
  xor	edi,	edi  
 @g_ok_hline_0:

 sub	ecx,	edi
 inc    ecx

 push 	ax
 mov	ax,	@y_g
 shl	ax,	6
 add	di,	ax
 shl	ax,	2
 add	di,	ax
 pop	ax

 @hline_g_loop:
  mov     edx,  eax
  sar     edx,   16
  mov es:[di],   dl
  add 	  eax,  ebx
  inc 	   di
 loop @hline_g_loop
 @_bye:
 pop bp
 ret 10
endp

@@dx12 DD ?
@@dx23 DD ?
@@dx13 DD ?

@@cx12 DD ?
@@cx23 DD ?
@@cx13 DD ?



@@c1  equ word ptr [bp+20]
@@c2  equ word ptr [bp+18]
@@c3  equ word ptr [bp+16]
@@c   equ word ptr [bp+16]
@@x1  equ word ptr [bp+14]
@@y1  equ word ptr [bp+12]
@@x2  equ word ptr [bp+10]
@@y2  equ word ptr [bp+ 8]
@@x3  equ word ptr [bp+ 6]
@@y3  equ word ptr [bp+ 4]

trian_ proc
 push bp
 mov bp, sp

 mov ax, @@y1
 cmp ax, @@y2
 jl @@ok1 
  xchg ax, @@y2
  mov @@y1, ax

  mov di, @@x1
  xchg di, @@x2
  mov @@x1, di
 @@ok1:

 mov ax, @@y2
 cmp ax, @@y3
 jl @@ok2
  xchg ax, @@y3
  mov @@y2, ax

  mov di, @@x2
  xchg di, @@x3
  mov @@x2, di
 @@ok2:

 mov ax, @@y1
 cmp ax, @@y2
 jl @@ok3
  xchg ax, @@y2
  mov @@y1, ax

  mov di, @@x1
  xchg di, @@x2
  mov @@x1, di
 @@ok3:

 cmp ax, @@y3
 jz @@bye
  
 mov ax, @@y3
 cmp ax, 0
 jle @@bye

 mov ax, @@y1
 cmp ax, 199
 jg  @@bye


 mov cx, @@y3
 sub cx, @@y1     ; y3-y1
 mov ax, @@x3
 sub ax, @@x1     ; x3-x1
 movsx ecx, cx    
 movsx eax, ax
 shl eax, 16      ; (x3-x1)*65536
 cdq   
 idiv ecx           
 mov @@dx13, eax  ; (x3-x1)*65536/(y3-y1)



 mov cx, @@y2
 cmp cx, 0
 jl @end_loop_12

  movsx ebx, @@x1
  shl ebx, 16

 sub cx, @@y1     ; y2-y1
 jz @end_loop_12


 mov ax, @@x2
 sub ax, @@x1     ; x2-x1
 movsx ecx, cx    
 movsx eax, ax
 shl eax, 16      ; (x2-x1)*65536
 cdq   
 idiv ecx           
 mov @@dx12, eax  ; (x2-x1)*65536/(y2-y1)

 mov cx, @@y2
 cmp cx, 200
 jl @ok_y2_200 
  mov cx, 200
  mov @@y2, cx
 @ok_y2_200:
 
 movsx eax, @@x1
 shl   eax, 16
 mov   ebx, eax
 mov   cx, @@y1

 cmp   cx, 0
jge @loop_12 
 neg cx
 movsx ecx, cx
 mov eax, @@dx13
 imul ecx           ; @@dx13*(-y1)
 add  ebx, eax

 mov eax, @@dx12
 imul ecx           ; @@dx12*(-y1)
 movsx ecx, @@x1
 shl  ecx, 16
 add  eax, ecx      ; eax= @@dx12*(-y1)+@@x1

 xor cx, cx
 cmp cx, @@y2
jz @end_loop_12 

 
@loop_12:
 push eax
 push cx

 sar eax, 16
 push ax        ; x1
 push cx        ; y
 mov  edx, ebx
 sar  edx, 16
 push dx        ; x2
 push @@c
 call hline

 pop cx
 pop eax
 
 add eax, @@dx12 
 add ebx, @@dx13

 inc cx

 cmp cx, @@y2
jne @loop_12
@end_loop_12:
 
 mov cx, @@y2
 cmp cx, 200
 je @@bye

 mov cx, @@y3
 sub cx, @@y2     ; y3-y2
 jz @@bye

  mov ax, @@x3
  sub ax, @@x2     ; x3-x2
  movsx ecx, cx    
  movsx eax, ax
  shl eax, 16      ; (x3-x2)*65536
  cdq   
  idiv ecx           
  mov @@dx23, eax  ; (x3-x2)*65536/(y3-y2)  

  mov cx, @@y3
  cmp cx, 200
  jl @ok_y3_200
   mov cx, 200
   mov @@y3,cx
  @ok_y3_200:

  movsx eax, @@x2
  shl   eax, 16 
  mov   cx, @@y2
 
  cmp cx, 0
  jge @loop_23 
    movsx ecx, @@y1 
    neg   ecx
    mov   eax, @@dx13
    imul  ecx          ; eax=dx13*(-y1)
    movsx ebx, @@x1
    shl   ebx, 16
    add   ebx, eax
    
    movsx ecx, @@y2 
    neg   ecx
    mov   eax, @@dx23
    imul  ecx          ; eax=dx23*(-y2)
    movsx ecx, @@x2
    shl   ecx, 16
    add   eax, ecx    


   xor 	  cx, cx

@loop_23:
 push eax
 push cx

 sar eax, 16
 push ax        ; x1
 push cx        ; y
 mov  edx, ebx
 sar edx, 16
 push dx        ; x2
 push @@c
 call hline

 pop cx
 pop eax
 
 add eax, @@dx23
 add ebx, @@dx13 

 inc cx
 cmp cx, @@y3
jnz @loop_23
 @@bye:
 pop bp
 ret 14
endp


pom DD ?
trian_g proc
 push bp
 mov bp, sp

 mov ax, @@y1
 cmp ax, @@y2
 jl @@g_ok1 
  xchg ax, @@y2
  mov @@y1, ax

  mov di, @@x1
  xchg di, @@x2
  mov @@x1, di

  mov di, @@c1
  xchg di, @@c2
  mov @@c1, di
 @@g_ok1:

 mov ax, @@y2
 cmp ax, @@y3
 jl @@g_ok2
  xchg ax, @@y3
  mov @@y2, ax

  mov di, @@x2
  xchg di, @@x3
  mov @@x2, di

  mov di, @@c2
  xchg di, @@c3
  mov @@c2, di
 @@g_ok2: 


 cmp @@y1, ax
 jl @@g_ok3
  xchg ax, @@y1
  mov @@y2, ax

  mov di, @@x1
  xchg di, @@x2
  mov @@x1, di

  mov di, @@c1
  xchg di, @@c2
  mov @@c1, di
 @@g_ok3:

 mov	ax,	@@y3
 cmp	ax,	0
 jl	@g_bye

 mov    ax,     @@y1
 cmp    ax,     @@y3
 jz     @g_bye          ; brak wysokoœci= koniec


; liczymy dla fazy 1->2 
 mov	cx,	@@y2
 sub	cx,	ax
 inc    cx
 jz @g_brak_loop_12
 movsx	ecx,	cx

 mov	ax, 	@@x2
 sub	ax, 	@@x1
 movsx	eax,	ax
 shl	eax,	16    ; eax = (x2-x1)*65536
 cdq
 idiv	ecx
 mov	@@dx12, eax

 mov	ax, 	@@c2
 sub	ax, 	@@c1
 movsx	eax,	ax
 shl	eax,	16    ; eax = (c2-c1)*65536
 cdq
 idiv	ecx
 mov	@@cx12, eax

 @g_brak_loop_12:
; liczymy dla fazy 1->3 
 mov	cx,	@@y3
 sub	cx,	@@y1
 inc    cx 
 movsx	ecx,	cx


 mov	ax, 	@@x3
 sub	ax, 	@@x1
 movsx	eax,	ax
 shl	eax,	16    ; eax = (x3-x1)*65536
 cdq
 idiv	ecx
 mov	@@dx13, eax

 mov	ax, 	@@c3
 sub	ax, 	@@c1
 movsx	eax,	ax
 shl	eax,	16    ; eax = (c3-c1)*65536
 cdq
 idiv	ecx
 mov	@@cx13, eax 

; liczymy dla fazy 2->3
 mov	cx,	@@y3
 sub	cx,	@@y2
 inc    cx
 jz 	@g_brak_loop_23
 movsx	ecx,	cx

 mov	ax, 	@@x3
 sub	ax, 	@@x2
 movsx	eax,	ax
 shl	eax,	16    ; eax = (x3-x2)*65536
 cdq
 idiv	ecx
 mov	@@dx23, eax

 mov	ax, 	@@c3
 sub	ax, 	@@c2
 movsx	eax,	ax
 shl	eax,	16    ; eax = (c3-c2)*65536
 cdq
 idiv	ecx
 mov	@@cx23, eax
 @g_brak_loop_23:    

; mamy wyliczone dx12, dx13, dx23, cx12, cx13, cx23

 movsx	edi, 	@@x1
 shl	edi, 	16
 mov	esi,	edi	; esi<-lewy(1->2); edi<-prawy(1->3)

 movsx	ebx, 	@@c1
 shl	ebx, 	16
 mov	ecx,	ebx	; ebx<-lewy(1->2); ecx<-prawy(1->3)


 mov	ax,	@@y2
 cmp	ax,	0       ; gdy y2<0 to zupe³na klapa dla fazy 1 =D
 jl	@g_2_brak_loop12

 mov	ax,	@@y1
 cmp	ax,	0
 jge	@g_ok_y1
  neg	ax
  movsx eax,	ax
  mov   pom,	eax
  imul	@@dx12		; (-y1)*dx12
  add	esi,	eax	; x1-=y1*dx12
  
  mov	eax,	@@dx13
  imul	dword ptr pom   
  add	edi,	eax

  mov	eax,	@@cx12
  imul	dword ptr pom   
  add	ebx,	eax

  mov	eax,	@@cx13
  imul	dword ptr pom   
  add	ecx,	eax

  xor	ax,	ax
  mov	@@y1,	ax
 @g_ok_y1:

 mov 	ax, 	@@y2
 cmp	ax,	200
 jl	@g_y2_mniejsze_200
  mov 	ax,	200
  mov	@@y2,	ax
 @g_y2_mniejsze_200:

 mov 	ax,	@@y1  
 ; esi, ebx(1->2)
 ; edi, ecx(1->3)


@g_loop_12:
 push	ax
 push	ebx
 push	edi
 push	ecx

 mov 	edx,	esi
 sar	edx,	16
 push	dx

 push	ax

 mov 	edx,	edi
 sar	edx,	16
 push	dx

 mov 	edx,	ebx
 sar	edx,	16
 push	dx

 mov 	edx,	ecx
 sar	edx,	16
 push	dx


 call	hline_g

 pop 	ecx
 pop 	edi
 pop	ebx
 pop	ax

 add	esi,	@@dx12
 add	edi,	@@dx13
 add	ebx,	@@cx12
 add	ecx,	@@cx13

 inc ax
 cmp ax, @@y2
jl @g_loop_12

@g_2_brak_loop12:
 mov	ax,	@@y3
 cmp	ax,	200
 jl	@@g_y3_ok_200
  mov	ax,	200
  mov	@@y3,	ax
 @@g_y3_ok_200:

 mov    ax,     @@y2 
 cmp    ax,     @@y3
 jz     @g_bye

 movsx	esi,	@@x2
 shl	esi,	16
 movsx	ebx,	@@c2
 shl	ebx,	16


 cmp	ax,	0
 jge	@g_y2_mniej_0
  neg	ax
  movsx	eax,	ax
  mov	pom,	eax
  imul	@@dx23
  add	esi,	eax

  mov	eax,	@@cx23
  imul	dword ptr pom
  add	ebx,	eax

  movsx	eax,	@@y1
  neg	eax
  mov	pom,	eax
  mov	eax,	@@dx13
  imul	dword ptr pom
  add	edi,	eax

  mov	eax,	@@cx13
  imul	dword ptr pom
  add	ecx,	eax

  xor	ax,	ax
 @g_y2_mniej_0:

@g_loop_23:
 push	ax
 push	ebx
 push	edi
 push	ecx

 mov 	edx,	esi
 sar	edx,	16
 push	dx

 push	ax

 mov 	edx,	edi
 sar	edx,	16
 push	dx

 mov 	edx,	ebx
 sar	edx,	16
 push	dx

 mov 	edx,	ecx
 sar	edx,	16
 push	dx


 call	hline_g

 pop 	ecx
 pop 	edi
 pop	ebx
 pop	ax

 add	esi,	@@dx23
 add	edi,	@@dx13
 add	ebx,	@@cx23
 add	ecx,	@@cx13

 inc ax
 cmp ax, @@y3
jl @g_loop_23
  

 @g_bye:

 pop bp
 ret 18
endp


pall proc
 mov al,1d
 mov dx,3c8h
 out dx,al
 mov dx,3c9h
 mov bx,1
 ppp:
  mov ax,bx
  out dx,al
  out dx,al
  out dx,al
  inc bx
 cmp bx,64d
 jne ppp

 ppp2:
  mov ax,bx
  sub ax,64
  out dx,al
  mov al,0
  out dx,al
  out dx,al
  inc bx
 cmp bx,128d
 jne ppp2

ret
endp

nowa_flaga:
 xor ecx, ecx
 lea di, point
 @f_pet_y:
  xor ebx, ebx
  @f_pet_x:

    mov tmp, ebx
    fild tmp
    mov eax, flagaX
    mov tmp, eax
    fisub tmp
    mov eax, Cell
    mov tmp, eax
    fimul tmp
    fstp d [di+@r_x]

    mov tmp, ecx      ;  (Y-(resY/2))*cell
    fild tmp        
    mov eax, flagaY
    mov tmp, eax
    fisub tmp
    mov eax, Cell
    mov tmp, eax
    fimul tmp
    fstp d [di+@r_z]

    mov eax, 10
    mov tmp, eax
    fild tmp
    fstp d [di+@r_y]

    add di, 16
 
    inc ebx
  cmp ebx, resX+1
  jne @f_pet_x
   inc ecx
 cmp ecx, (resY+1)
 jne @f_pet_y

 lea di, faces
 xor ecx, ecx
 xor edx, edx
 @face_pet_y:
  xor ebx, ebx
  push dx              
  @face_pet_x:
   push dx           
   mov [di], dx
   add dx, 16
   mov [di+2], dx
   mov [di+6], dx
   add dx, 256
   mov [di+4], dx
   mov [di+8], dx
   add dx, 16
   mov [di+10], dx
   pop dx              
   add dx, 16          
   add di, 12
   inc ebx 
  cmp ebx, resX
  jnz @face_pet_x
  pop dx
  add dx, 272
  inc ecx
 cmp ecx, resY
 jnz @face_pet_y
ret


z    dd ?
l256 dd 256.0
l128 dd 128.0
sx   dd 160.0
sy   dd 100.0
a    dd 0.0
adda dd 0.1

l5   dd -17.0
l6   dd 0.33
l3   dd 0.3
l4   dd 0.3;


flaga_to_2d:
 mov ecx, size_p
 shr ecx, 4
 lea di, point
 lea si, matrix
 @loop_2d:
   fld  d [di+@r_x]
   fmul d [di+@r_x]
   fld  d [di+@r_z]
   fmul d [di+@r_z]
   faddp st(1),st
   fsqrt
   fsub a
   fsin
   fmul l3
   fstp d [di+@r_y]

   fld d [di+@r_x]
   fadd l4
   fld st
   fmulp st(1),st 
   fld d [di+@r_z]
   fadd l5
   fld st
   fmulp st(1),st 
   faddp st(1),st
   fsqrt         
   fmul l6 
   fsub a
   fsin
   mov eax, 2     ;
   mov tmp,eax    ;wieksze fale
   fimul tmp      ;
   fadd d [di+@r_y]
   fstp d [di+@r_y]  
   fld d [di+@r_x]
   fld d [di+@r_y]
   fld d [di+@r_z]   
   fstp zz
   fstp yy
   fstp xx
 
   call mul_m

  fld d zz
  fadd l128
  fstp z

  fld d xx
  fmul l256
  fdiv z
  fadd d sx
  fistp tmp
  mov eax, tmp
  mov [di+@r_dx], ax

  fld d yy
  fmul l256
  fdiv z
  fadd d sy
  fistp tmp
  mov eax, tmp
  mov [di+@r_dy], ax
  add di,16  
  dec cx
 jnz @loop_2d
ret


; di <- matrix;
czysc_m proc
 push di
 fldz
 fstp tmp
 mov eax, tmp
 mov cx, 16
 rep stosd
 pop di
 fld1
 fst  d [di   ]
 fst  d [di+20]
 fst  d [di+40]
 fstp d [di+60]
 ret
endp

xx dd ?
yy dd ?
zz dd ?
; si <- matrix
mul_m proc
 fld xx
 fmul d [si   ]
 fld yy
 fmul d [si+16]
 fld zz
 fmul d [si+32]
 fadd
 fadd                   ; x

 fld xx
 fmul d [si+ 4]
 fld yy
 fmul d [si+20]
 fld zz
 fmul d [si+36]
 fadd
 fadd                   ; y

 fld xx
 fmul d [si+ 8]
 fld yy
 fmul d [si+24]
 fld zz
 fmul d [si+40]
 fadd
 fadd                   ; z

 fstp zz
 fstp yy
 fstp xx
 ret
endp

 rx   dd 0.872222222
 ry   dd 0.0
addry dd 0.01

@cos_x dd ?
@cos_y dd ?
@sin_x dd ?
@sin_y dd ?

; lea si, matrix
rot_yx proc
 fld rx
 fsincos
 fstp @cos_x 
 fstp @sin_x

 fld ry
 fsincos
 fstp @cos_y 
 fstp @sin_y

 fld @cos_y
 fstp d [si]

 fld @sin_y
 fmul @sin_x
 fstp d [si+4]

 fld @sin_y
 fchs
 fmul @cos_x
 fstp d [si+8]

 fldz
 fstp d [si+16]

 fld @cos_x
 fstp d [si+20]

 fld @sin_x
 fstp d [si+24]

 fld @sin_y
 fstp d [si+32]

 fld  @sin_x
 fchs
 fmul @cos_y
 fstp d [si+36]

 fld @cos_y
 fmul @cos_x
 fstp d [si+40]
 ret
endp

 point  db size_p dup(?)       
 faces  dw faces_*3 dup(?)  ; w1, w2, w3 
 matrix dd 16 dup(?)
code16 ends

MY_STACK segment para stack 'stack'
 db STACK_SIZE dup(?)
MY_STACK ends

end start16
