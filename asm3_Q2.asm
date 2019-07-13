; Name: Tomer Guttman
; ID: 
; Date: 2/6/2019

INCLUDE Irvine32.inc
INCLUDE asm3_Q2_data.inc

.data
studentInfo BYTE "Tomer Guttman, 204381487",10,13,0
BoardT BYTE Lengthof Board dup (?) ;prepare the copy of the original Board.
Path BYTE Lengthof Board dup (?);prepare the final path presentation.
boardFinalMSG BYTE "This is the representation of the Board in Memory: ",10,13,0;
lengthFinalMSG BYTE "The path length is: ",0
pathOutputMSG BYTE "The path from S to E is: ",0
checkPathFinalMSG BYTE "Calling checkPath, returns: ",0

.code
;===========================findStart=========================================================
;this function recieves the starting address of the input board,n_cols and n_rows.
;the function returns the cordinates of the starting cell in the input board (ebx,eax) = (x,y)
;ebx - represent the x cordinate in the maze. n_cols.
;eax - represent the y cordinate in the maze. n_rows.
;edx - holds the offset of the cell we begin the maze from.
;ecx - holds the value of the current cell in the input board.
;edi - holds the offset of the start of the board.
;edx - holds the value which represent the index of the current cell in the input board.
;=============================================================================================
findStart PROC
	push ebp
	mov ebp, esp
	push ecx
	push edi
	xor eax, eax
	xor ebx, ebx
	xor ecx, ecx
	xor edx, edx
	mov edi, [ebp + 12]
	loop1:
		mov cl, [edi + edx]
		cmp cl, 'S'
		jz DONE
		inc edx										
		inc ebx
		cmp bx, [ebp + 10] 
		jnz loop1
		mov ebx, 0
		inc eax
		jmp loop1
	DONE: 
	pop edi
	pop ecx
	mov esp, ebp
	pop ebp	
	ret 8 
findStart ENDP
								
;========================================checkPath=============================================================
;the current function recieves a Board,amount of cols, amount of rows and a path to check.
;after going through the given input the function update the update according to the path status:
;'S' - Success
;'F' - Fail
;'I' - Illegal
;esi - acts as a temporary variable.
;edx - holds the current move in the given path array.
;ecx - acts as an indexer to 'walk' on the path array.
;edi - holds the starting address of the input Board.
;eax - holds the value in the current cell were working with.
;ebx - holds the index of the current cell in the input Board array.
;==============================================================================================================
checkPath PROC
	push ebp
	mov ebp, esp
	sub esp, 4
	push ecx
	push esi
	push edi
	push ebx
	push edx
	push DWORD ptr [ebp + 16] ; push Board address  
	push WORD ptr [ebp + 14]	; push n_cols
	push WORD ptr [ebp + 12] ; push n_rows
	call findStart
	mov WORD ptr [ebp - 2], ax 
	mov WORD ptr [ebp - 4], bx 
	mov ebx, edx
	mov edx, 0
	mov edi, [ebp + 16]
	mov ecx, 0
	mov eax, 0
	;using a loop to go through the whole path, step by step.
	loop1: 
		mov esi, [ebp + 8]
		mov dl, [esi + ecx] ; get the next move in the input path array.
		cmp edx, 0			;check if it's the end of the path.
		jz ENDING
		cmp edx, 'L'		
		jz LEFT
		cmp edx, 'R'		
		jz RIGHT 
		cmp edx, 'U'		
		jz UP
		DOWN:			
			add bx, [ebp + 14]
			inc WORD ptr [ebp -2]
			jmp CHECK
		UP:
			sub bx, [ebp + 14]
			dec WORD ptr [ebp - 2]
			jmp CHECK
		LEFT:
			dec bx
			dec WORD ptr[ebp - 4]
			jmp CHECK
		RIGHT:
			Inc bx
			inc WORD ptr[ebp - 4]
		;check if isn't a 'blocked' cell.
		CHECK:
			push WORD ptr [ebp - 2] ; push current row index 
			push WORD ptr [ebp - 4]	; push current column index 
			push WORD ptr [ebp + 12] ; push n_rows
			push WORD ptr [ebp + 14] ; push n_cols
			call checkBounderies
			cmp eax, 0 		; if equals were out of the maze bounderies.
			je ILLEGAL
			mov al, [edi + ebx]
			cmp al, 1	; if we 'hit' a black cell.
			je ILLEGAL
			inc ecx
			jmp loop1
	ILLEGAL:
		mov al, 'I'
		jmp ENDPROC
	ENDING:
		cmp al, 'E' 
		jnz FAILURE
		mov al, 'S'
		jmp ENDPROC
	FAILURE:
		mov al, 'F'
	ENDPROC:
		pop edx
		pop ebx
		pop edi
		pop esi
		pop ecx
		mov esp, ebp
		pop ebp
		ret 12
checkPath ENDP

;===================================FindCellIndex==============================================================
;the current function recieves the following: row,col and column limitation.
;after calculation the function returns using the edx register, the index of the cell we were looking for in
;the input Board array - we need to manipulate the input to get the right index.
;==============================================================================================================
FindCellIndex PROC
	push ebp 
	mov ebp, esp
	push ecx
	movsx edx, WORD ptr [ebp + 12]
	cmp edx, 0 
	JE addColumn
	movsx ecx, WORD ptr [ebp + 8]
	dec ecx
	MULT:
		add dx, WORD ptr [ebp + 12]
	loop MULT
	addColumn:
		add dx, WORD ptr [ebp + 10]
	pop ecx
	mov esp, ebp
	pop ebp
	ret 6
FindCellIndex ENDP

;============================================checkBounderies=========================================
;the current function recieves the follwing: row, col , limit of row and limit of cols.
;the function check and output 0 if the checked cell is not in the maze bounderies and 1 if it is.
;ecx - holds the input col.
;ebx - holds the input row
;====================================================================================================
checkBounderies PROC 
	push ebp
	mov ebp, esp
	push ebx
	push ecx
	mov ebx, 0
	mov ecx, 0
	mov bx, WORD ptr [ebp + 14]
	mov cx, WORD ptr[ebp + 12]
	cmp bx, 0;row >= 0
	JL IsntInBounderies
	cmp bx, WORD ptr[ebp + 10];row < row limitation
	jge IsntInBounderies
	cmp cx, 0;col >= 0
	JL IsntInBounderies
	cmp cx, WORD ptr[ebp + 8];col < col limitation
	jge IsntInBounderies
	mov eax, 1
	jmp DONE
	IsntInBounderies:
		mov eax, 0
	DONE:
		pop ecx
		pop ebx
		mov esp, ebp
		pop ebp
		ret 8
checkBounderies ENDP

;==================================findpath_r=====================================================================================
;the current function acts as a recuresive one - which finds the right course of path in the input maze - if exists!
;findpath_r revieves the following input: n_cols,n_rows,x,y,offset path and the current move in the given path.
;the function finds and update the path array with the right moves to exit the maze and update the length of the path accordingly.
;edx - holds the index of the current cell in the input Board array.
;esi - holds the address of the given Board.
;ebx(bl) - holds the value in the current Board cell.
;ecx - holds the current offset were using.
;edi - holds the offset of Path array which represent the right moves to exit the maze.
;=================================================================================================================================
findpath_r PROC
	push ebp 
	mov ebp, esp
	push esi
	push ebx
	push edx
	push edi
	push ecx
	mov esi, DWORD ptr [ebp + 22] ; save Board address
	push WORD ptr [ebp + 14] ;push current row
	push WORD ptr [ebp + 16] ;push current col
	push WORD ptr [ebp + 18] ;push row limitation
	push WORD ptr [ebp + 20] ;push col limitation
	call checkBounderies
	
	cmp eax, 0
	JE HITBLACK
	push WORD ptr [ebp + 14] 
	push WORD ptr [ebp + 16] 
	push WORD ptr [ebp + 20] 
	call FindCellIndex	 
	
	mov ebx, 0
	mov bl, BYTE ptr [esi + edx]
	cmp bl, 'E'			
	je ENDING 
	cmp bl, 1			
	JE HITBLACK
	mov BYTE ptr [esi + edx], 1
	mov edi, DWORD ptr [ebp + 10] 
	movsx ecx, WORD ptr [ebp + 8] 
	mov BYTE ptr [edi + ecx], 'R'
	push esi					
	push WORD ptr [ebp + 20]	
	push WORD ptr [ebp + 18]	
	mov ax, WORD ptr[ebp + 16]
	inc ax
	push ax					;push the current col + 1
	push WORD ptr[ebp + 14]	;push the current row
	push edi				;push the path offset
	inc cx					
	push cx		;push offset + 1
	dec cx	
	call findpath_r
	
	cmp eax, -1
	jne ENDINGPROC
	mov BYTE ptr [edi + ecx], 'L'
	push esi					; push Board address
	push WORD ptr [ebp + 20]	; push n_col
	push WORD ptr [ebp + 18]	; push n_row
	mov ax, WORD ptr[ebp + 16]
	dec ax
	push ax						; push current Col - 1
	push WORD ptr[ebp + 14]		; push current row
	push edi					; push Path address
	inc cx					
	push cx						;push offset + 1
	dec cx	
	call findpath_r		
	cmp eax, -1
	jne ENDINGPROC
	mov BYTE ptr [edi + ecx], 'D'
	push esi					
	push WORD ptr [ebp + 20]
	push WORD ptr [ebp + 18]	
	push WORD ptr [ebp + 16] 	
	mov ax, WORD ptr[ebp + 14]
	inc ax
	push ax						; push current row + 1
	push edi					; push Path address
	inc cx					
	push cx						; push offset + 1
	dec cx	
	call findpath_r			
	cmp eax, -1
	jne ENDINGPROC
	mov BYTE ptr [edi + ecx], 'U'
	push esi					
	push WORD ptr [ebp + 20]	
	push WORD ptr [ebp + 18]	
	push WORD ptr [ebp + 16] 
	mov ax, WORD ptr[ebp + 14]
	dec ax
	push ax		; push current row - 1
	push edi	; push Path address
	inc cx					
	push cx		; push offset + 1
	dec cx	
	call findpath_r			
	cmp eax, -1
	jne ENDINGPROC
	jmp HITBLACK
	ENDING:
		movsx eax, WORD ptr [ebp + 8]	;end of path, return current path length
		jmp ENDINGPROC
	HITBLACK:
		mov eax, -1	;illegal path
	ENDINGPROC:
		pop ecx
		pop edi
		pop edx
		pop ebx
		pop esi
		mov esp, ebp
		pop ebp
		ret 18
findpath_r ENDP


;=======================================find_path===============================================================================
;The current function recieves the following input: the offset of the maze Board,n_cols,n_rows and the offset of the path array.
;then the function check if a legal path exists from S to E.
;if a path exists the function update the path array with the right course to exit the maze and update the length of the path.
;otherwise the function returns -1.
;===============================================================================================================================
find_path PROC
	push ebp
	mov ebp, esp
	push ebx
	push edx
	push ecx
	push DWORD ptr [ebp + 16]	; push Board address
	push WORD ptr [ebp + 14]	; push n_col
	push WORD ptr [ebp + 12]	; push n_row
	call findStart
	push DWORD ptr [ebp + 16]	; push Board offset
	push WORD ptr [ebp + 14]	; push n_cols
	push WORD ptr [ebp + 12]	; push n_rows
	push WORD ptr bx			; push the start position column
	push WORD ptr ax 			; push the start position row
	push DWORD ptr [ebp + 8]	; push Path offset
	push WORD ptr 0				;push starting offset 
	call findpath_r
	pop ecx
	pop edx 
	pop ebx	
	mov esp, ebp
	pop ebp
	ret 12
find_path ENDP

;=================================copyArray=======================================================================
;the function recieves the following input: offset of the array, the offset of the additional array and their size.
;the function copies the information from one array to the other one.
;ecx - holds the length of the array - used as a loop counter.
;eax - holds the offset of the array you want to copy from.
;ebx - holds the offset of the array you want to copy to.
;esi - holds the array inx.
;edx - holds the current value we want to copy.
;=================================================================================================================
copyArray PROC
	push ebp
	mov ebp, esp
	push eax
	push ebx
	push ecx
	push edx
	push esi
	mov esi, 0
	mov edx, 0
	mov eax ,DWORD ptr [ebp + 14];eax holds Board offset
	mov ebx ,DWORD ptr [ebp + 10];ebx holds BoardT offset
	movsx ecx, WORD ptr[ebp + 8] ;ecx holds boards size
	COPY:
	mov dl, [eax + esi]
	mov [ebx + esi], dl
	inc esi	
	loop COPY
	pop esi
	pop edx
	pop ecx 
	pop ebx
	pop eax
	mov esp, ebp
	pop ebp
	ret 10
copyArray ENDP	


myMain PROC
	mov edx, offset studentInfo
	call Writestring
	push offset Board
	push offset BoardT
	push WORD ptr lengthof Board
	call copyArray
	push offset BoardT
	push n_cols
	push n_rows
	push offset Path
	call find_path

	mov edx, offset boardFinalMSG
	call writeString
	mov esi, offset Board
	mov ebx, 1
	mov ecx, lengthof Board
	call DumpMem

	cmp eax, -1
	je NOSHOW

	mov edx, offset pathOutputMSG
	call writeString
	push eax	
	mov ecx, eax
	mov ebx, 0
	PRINTPATH:
		movsx eax, Path[ebx]
		call writeChar
		inc ebx
	loop PRINTPATH
	
	pop eax	
	call crlf
	NOSHOW:
		mov edx, offset lengthFinalMSG
		call writeString
		call writeInt
		call crlf
		
	mov edx, offset checkPathFinalMSG
	call writeString
	push offset Board
	push n_cols
	push n_rows
	push offset Path
	call checkPath
	call writeChar
	call ExitProcess
myMain ENDP
END myMain