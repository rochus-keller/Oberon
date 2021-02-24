MODULE HennessyV4;
(*  This is a suite of benchmarks that are relatively short, both in program
    size and execution time.  It requires no input, and prints the execution
    time for each program, using the system- dependent routine Getclock,
    below, to find out the current CPU time.  It does a rudimentary check to
    make sure each program gets the right output.  These programs were
    gathered by John Hennessy and modified by Peter Nye.
    Oberon: J.Templ 26.2.90 
    
    Source: https://github.com/jtempl/ofront, V4_ofront/share, commit 46f758c on 2 Jan 2020
    2021-02-22 Rochus Keller: renamed to HennessyV4; added BEGIN main;
    
    *)

IMPORT
	Oberon, Out;

CONST
	bubblebase = 1.61;
	dnfbase = 3.5;
	permbase = 1.75;
	queensbase = 1.83;
	towersbase = 2.39;
	quickbase = 1.92;
	intmmbase = 1.46;
	treebase =  2.5;
	mmbase = 0.0 (* 0.73 *);
	fpmmbase = 2.92;
	puzzlebase = 0.5;
	fftbase = 0.0 (* 1.11 *);
	fpfftbase = 4.44;

	(* Towers *)
	maxcells = 18;
	stackrange = (*0..*) 3;

	(* Intmm, Mm *)
	rowsize = 40;

	(* Puzzle *)
	size = 511;
	classmax = 3;
	typemax = 12;
	d = 8;

	(* Bubble, Quick *)
	sortelements = 5000;
	srtelements = 500;

	(* fft *)
	fftsize = 256;
	fftsize2 = 129;

	(* Perm *)
	permrange = (*0 ..*)10;
    (* Towers *)

TYPE
	(* tree *)
	node = POINTER TO nodeDesc;
	nodeDesc = RECORD
		left, right: node;
		val: LONGINT;
	END;
	
	(* Towers
	discsizrange = 1..maxcells;
	cellcursor = 0..maxcells; *)
	element = RECORD
		discsize: LONGINT;
		next: LONGINT;
	END ;
	
(*    emsgtype = packed array[1..15] of char;
*)
    (* Intmm, Mm *) (*
    index = 1 .. rowsize; *)
    intmatrix = ARRAY rowsize+1,rowsize+1 OF LONGINT;
    realmatrix = ARRAY rowsize+1,rowsize+1 OF REAL;

    (* Puzzle *) (*
    piececlass = 0..classmax;
    piecetype = 0..typemax;
    position = 0..size;
*)
    (* Bubble, Quick *) (*
    listsize = 0..sortelements;
    sortarray = array [listsize] of integer;
*)
    (* FFT *)
	complex = RECORD
		rp, ip: REAL
	END;
    carray = ARRAY fftsize+1 OF complex ;
    c2array = ARRAY fftsize2+1 OF complex ;

	Proc = PROCEDURE;
	
VAR
	fixed,floated: REAL;

    (* global *)
	seed: LONGINT;

    (* Perm *)
	permarray: ARRAY permrange+1 OF LONGINT;
	pctr: LONGINT;

    (* tree *)
	tree: node;

	(* Towers *)
    stack: ARRAY stackrange+1 OF LONGINT;
	cellspace: ARRAY maxcells+1 OF element;
	freelist: LONGINT;
	movesdone: LONGINT;

	(* Intmm, Mm *)
	ima, imb, imr: intmatrix;
	rma, rmb, rmr: realmatrix;

	(* Puzzle *)
	piececount: ARRAY classmax+1 OF LONGINT;
	class, piecemax: ARRAY typemax+1 OF LONGINT;
	puzzl: ARRAY size+1 OF BOOLEAN;
	p: ARRAY typemax+1, size+1 OF BOOLEAN;
	n,
	kount: LONGINT;

	(* Bubble, Quick *)
	sortlist: ARRAY sortelements+1 OF LONGINT;
    biggest, littlest,
    top: LONGINT;

    (* FFT *)
	z, w: carray;
    e: c2array;
	zr, zi: REAL;

(* global procedures *)

	PROCEDURE Getclock (): LONGINT;
	BEGIN RETURN Oberon.Time()
	END Getclock;
	
	PROCEDURE Initrand ();
	BEGIN seed := 74755
	END Initrand;
	
	PROCEDURE Rand (): LONGINT;
	BEGIN
     seed := (seed * 1309 + 13849) MOD 65535;
     RETURN (seed);
	END Rand;


    (* Permutation program, heavily recursive, written by Denny Brown. *)
	
	PROCEDURE Swap (VAR a,b: LONGINT);
		VAR t: LONGINT;
	BEGIN t := a;  a := b;  b := t;
	END Swap;
	
	PROCEDURE Initialize ();
		VAR i: LONGINT;
	BEGIN i := 1;
		WHILE i <= 7 DO
			permarray[i] := i-1;
			INC(i)
		END
	END Initialize;
	
	PROCEDURE Permute (n: LONGINT);
		VAR k: LONGINT;
	BEGIN
		pctr := pctr + 1;
		IF ( n#1 ) THEN
			Permute(n-1);
			k := n-1;
			WHILE k >= 1 DO
				Swap(permarray[n], permarray[k]);
				Permute(n-1);
				Swap(permarray[n], permarray[k]);
				DEC(k)
			END
       END
	END Permute;
	
	PROCEDURE *Perm ();
		VAR i: LONGINT;
	BEGIN
		pctr := 0; i := 1;
		WHILE i <= 5 DO
			Initialize();
			Permute(7);
			INC(i)
		END ;
		IF ( pctr # 43300 ) THEN Out.String(" Error in Perm."); Out.Ln END
	END Perm;


    (*  Program to Solve the Towers of Hanoi *)

	PROCEDURE Makenull (s: LONGINT);
	BEGIN stack[s] := 0
	END Makenull;

	PROCEDURE Getelement (): LONGINT;
		VAR temp: LONGINT;
	BEGIN
		IF ( freelist>0 ) THEN
			temp := freelist;
			freelist := cellspace[freelist].next;
		ELSE
			Out.String("out of space   "); Out.Ln
		END ;
		RETURN (temp);
	END Getelement;
	
	PROCEDURE Push(i,s: LONGINT);
        VAR localel: LONGINT; errorfound: BOOLEAN;
	BEGIN
		errorfound := FALSE;
		IF ( stack[s] > 0 ) THEN
			IF ( cellspace[stack[s]].discsize<=i ) THEN
				errorfound := TRUE;
				Out.String("disc size error"); Out.Ln
			END 
		END ;
		IF ( ~ errorfound ) THEN
			localel := Getelement();
			cellspace[localel].next := stack[s];
			stack[s] := localel;
			cellspace[localel].discsize := i
		END
	END Push;
	
	PROCEDURE Init (s,n: LONGINT);
		VAR discctr: LONGINT;
	BEGIN
		Makenull(s); discctr := n;
		WHILE discctr >= 1 DO
			Push(discctr,s);
			DEC(discctr)
		END
	END Init;

	PROCEDURE Pop (s: LONGINT): LONGINT;
		VAR temp, temp1: LONGINT;
	BEGIN
		IF ( stack[s] > 0 ) THEN
			temp1 := cellspace[stack[s]].discsize;
			temp := cellspace[stack[s]].next;
			cellspace[stack[s]].next := freelist;
			freelist := stack[s];
			stack[s] := temp;
			RETURN (temp1)
		ELSE
			Out.String("nothing to pop "); Out.Ln
		END
	END Pop;

	PROCEDURE Move (s1,s2: LONGINT);
	BEGIN
		Push(Pop(s1),s2);
		movesdone := movesdone+1;
	END Move;

	PROCEDURE tower(i,j,k: LONGINT);
		VAR other: LONGINT;
	BEGIN
		IF ( k=1 ) THEN
			Move(i,j);
		ELSE
			other := 6-i-j;
			tower(i,other,k-1);
			Move(i,j);
			tower(other,j,k-1)
		END
	END tower;

	PROCEDURE *Towers ();
		VAR i: LONGINT;
	BEGIN i := 1;
		WHILE i <= maxcells DO cellspace[i].next := i-1; INC(i) END ;
		freelist := maxcells;
		Init(1,14);
		Makenull(2);
		Makenull(3);
		movesdone := 0;
		tower(1,2,14);
		IF ( movesdone # 16383 ) THEN Out.String(" Error in Towers."); Out.Ln END
	END Towers;


    (* The eight queens problem, solved 50 times. *)
(*
  type
      doubleboard =   2..16;
      doublenorm  =   -7..7;
      boardrange  =   1..8;
      aarray      =   array [boardrange] of boolean;
      barray      =   array [doubleboard] of boolean;
      carray      =   array [doublenorm] of boolean;
      xarray      =   array [boardrange] of boardrange;
*)

	PROCEDURE Try(i: LONGINT; VAR q: BOOLEAN; VAR a, b, c: ARRAY OF BOOLEAN; VAR x: ARRAY OF LONGINT);
    	VAR j: LONGINT;
	BEGIN
		j := 0;
		q := FALSE;
		WHILE (~q) & (j # 8) DO
			j := j + 1;
			q := FALSE;
			IF b[j] & a[i+j] & c[i-j+7] THEN
				x[i] := j;
				b[j] := FALSE;
				a[i+j] := FALSE;
				c[i-j+7] := FALSE;
				IF i < 8 THEN
					Try(i+1,q,a,b,c,x);
					IF ~q THEN
						b[j] := TRUE;
						a[i+j] := TRUE;
						c[i-j+7] := TRUE
					END
     	   	ELSE q := TRUE
				END
			END
		END
	END Try;

	PROCEDURE Doit ();
		VAR i: LONGINT; q: BOOLEAN;
			a: ARRAY 9 OF BOOLEAN;
			b: ARRAY 17 OF BOOLEAN;
			c: ARRAY 15 OF BOOLEAN;
			x: ARRAY 9 OF LONGINT;
	BEGIN
		i := 0 - 7;
		WHILE i <= 16 DO
			IF (i >= 1) & (i <= 8) THEN a[i] := TRUE END ;
			IF i >= 2 THEN b[i] := TRUE END ;
			IF i <= 7 THEN c[i+7] := TRUE END ;
			i := i + 1;
		END ;
		Try(1, q, b, a, c, x);
		IF ( ~ q ) THEN Out.String(" Error in Queens."); Out.Ln END
	END Doit;

	PROCEDURE *Queens ();
		VAR i: LONGINT;
	BEGIN i := 1;
		WHILE i <= 50 DO Doit(); INC(i) END
	END Queens;


    (* Multiplies two integer matrices. *)

	PROCEDURE Initmatrix (VAR m: intmatrix);
		VAR temp, i, j: LONGINT;
	BEGIN i := 1;
		WHILE i <= rowsize DO
			j := 1;
			WHILE j <= rowsize DO
				temp := Rand();
				m[i][j] := temp - (temp DIV 120)*120 - 60;
				INC(j)
			END ;
			INC(i)
		END
	END Initmatrix;

	PROCEDURE Innerproduct(VAR result: LONGINT; VAR a,b: intmatrix; row,column: LONGINT);
		VAR i: LONGINT;
  (* computes the inner product of A[row,*] and B[*,column] *)
	BEGIN
		result := 0; i := 1;
		WHILE i <= rowsize DO result := result+a[row][i]*b[i][column]; INC(i) END
	END Innerproduct;

	PROCEDURE *Intmm ();
		VAR i, j: LONGINT;
	BEGIN
		Initrand();
		Initmatrix (ima);
		Initmatrix (imb);
		i := 1;
		WHILE i <= rowsize DO j := 1;
			WHILE j <= rowsize DO Innerproduct(imr[i][j],ima,imb,i,j); INC(j) END ;
			INC(i)
		END
	END Intmm;


    (* Multiplies two real matrices. *)

	PROCEDURE rInitmatrix (VAR m: realmatrix);
		VAR temp, i, j: LONGINT;
	BEGIN i := 1;
		WHILE i <= rowsize DO j := 1;
			WHILE j <= rowsize DO
				temp := Rand();
				m[i][j] := (temp - (temp DIV 120)*120 - 60) DIV 3;
				INC(j)
			END ;
			INC(i)
		END
	END rInitmatrix;

	PROCEDURE rInnerproduct(VAR result: REAL; VAR a,b: realmatrix; row,column: LONGINT);
	(* computes the inner product of A[row,*] and B[*,column] *)
		VAR i: LONGINT;
	BEGIN
		result := 0.0; i := 1;
		WHILE i<=rowsize DO result := result+a[row][i]*b[i][column]; INC(i) END
	END rInnerproduct;

	PROCEDURE *Mm ();
		VAR i, j: LONGINT;
	BEGIN
		Initrand();
		rInitmatrix (rma);
		rInitmatrix (rmb);
		i := 1;
		WHILE i <= rowsize DO j := 1;
			WHILE j <= rowsize DO rInnerproduct(rmr[i][j],rma,rmb,i,j); INC(j) END ;
			INC(i)
		END
	END Mm;


    (* A compute-bound program from Forest Baskett. *)

	PROCEDURE Fit (i, j: LONGINT): BOOLEAN;
		VAR k: LONGINT;
	BEGIN k := 0;
		WHILE k <= piecemax[i] DO
			IF ( p[i][k] ) THEN IF ( puzzl[j+k] ) THEN RETURN FALSE END END;
			INC(k)
		END;
		RETURN TRUE
	END Fit;
	
	PROCEDURE Place (i, j: LONGINT): LONGINT;
		VAR k: LONGINT;
	BEGIN k := 0;
		WHILE k <= piecemax[i] DO
			IF ( p[i][k] ) THEN puzzl[j+k] := TRUE END;
			INC(k)
		END;
		piececount[class[i]] := piececount[class[i]] - 1;
		k := j;
		WHILE k <= size DO
			IF ( ~ puzzl[k] ) THEN RETURN (k) END;
			INC(k)
		END ;
		RETURN (0);
	END Place;
	
	PROCEDURE Remove (i, j: LONGINT);
		VAR k: LONGINT;
	BEGIN k := 0;
		WHILE k <= piecemax[i] DO
			IF ( p[i][k] ) THEN puzzl[j+k] := FALSE END;
			INC(k)
		END;
		piececount[class[i]] := piececount[class[i]] + 1
	END Remove;
	
	PROCEDURE Trial (j: LONGINT): BOOLEAN;
		VAR i, k: LONGINT;
	BEGIN i := 0;
		kount := kount + 1;
		WHILE i <= typemax DO
			IF ( piececount[class[i]] # 0 ) THEN
				IF ( Fit (i, j) ) THEN
					k := Place (i, j);
					IF Trial(k) OR (k = 0) THEN RETURN (TRUE)
					ELSE Remove (i, j)
					END;
				END
			END;
			INC(i)
		END;
		RETURN (FALSE)
	END Trial;
	
	PROCEDURE* Puzzle ();
		VAR i, j, k, m: LONGINT;
	BEGIN
		m := 0; WHILE m <= size DO puzzl[m] := TRUE; INC(m) END ;
		i := 1;
		WHILE i <= 5 DO j := 1;
			WHILE j <= 5 DO k := 1;
				WHILE k <= 5 DO
					puzzl[i+d*(j+d*k)] := FALSE; INC(k)
				END;
				INC(j)
			END;
			INC(i)
		END;
		
		i := 0; 
		WHILE i <= typemax DO m := 0;
			WHILE m<= size DO
				p[i][m] := FALSE; INC(m)
			END;
			INC(i)
		END;
		
		i := 0;
		WHILE i <= 3 DO j := 0;
			WHILE j <= 1 DO k := 0;
				WHILE k <= 0 DO
					p[0][i+d*(j+d*k)] := TRUE; INC(k)
				END;
				INC(j)
			END;
			INC(i)
		END;
		class[0] := 0;
		piecemax[0] := 3+d*1+d*d*0;
		
		i := 0;
		WHILE i <= 1 DO j := 0;
			WHILE j <= 0 DO k := 0;
				WHILE k <= 3 DO
					p[1][i+d*(j+d*k)] := TRUE; INC(k)
				END;
				INC(j)
			END;
			INC(i)
		END;
		class[1] := 0;
		piecemax[1] := 1+d*0+d*d*3;
		
		i := 0;
		WHILE i <= 0 DO j := 0;
			WHILE j <= 3 DO k := 0;
				WHILE k <= 1 DO
					p[2][i+d*(j+d*k)] := TRUE; INC(k)
				END;
				INC(j)
			END;
			INC(i)
		END;
		class[2] := 0;
		piecemax[2] := 0+d*3+d*d*1;
		
		i := 0;
		WHILE i <= 1 DO j := 0;
			WHILE j <= 3 DO k := 0;
				WHILE k <= 0 DO
					p[3][i+d*(j+d*k)] := TRUE; INC(k)
				END;
				INC(j)
			END;
			INC(i)
		END;
		class[3] := 0;
		piecemax[3] := 1+d*3+d*d*0;
	
		i := 0;
		WHILE i <= 3 DO j := 0;
			WHILE j <= 0 DO k := 0;
				WHILE k <= 1 DO
					p[4][i+d*(j+d*k)] := TRUE; INC(k)
				END;
				INC(j)
			END;
			INC(i)
		END;
		class[4] := 0;
		piecemax[4] := 3+d*0+d*d*1;
		
		i := 0;
		WHILE i <= 0 DO j := 0;
			WHILE j <= 1 DO k := 0;
				WHILE k <= 3 DO
					p[5][i+d*(j+d*k)] := TRUE; INC(k)
				END;
				INC(j)
			END;
			INC(i)
		END;
		class[5] := 0;
		piecemax[5] := 0+d*1+d*d*3;
		
		i := 0;
		WHILE i <= 2 DO j := 0;
			WHILE j <= 0 DO k := 0;
				WHILE k <= 0 DO
					p[6][i+d*(j+d*k)] := TRUE; INC(k)
				END;
				INC(j)
			END;
			INC(i)
		END;
		class[6] := 1;
		piecemax[6] := 2+d*0+d*d*0;
		
		i := 0;
		WHILE i <= 0 DO j := 0;
			WHILE j <= 2 DO k := 0;
				WHILE k <= 0 DO
					p[7][i+d*(j+d*k)] := TRUE; INC(k)
				END;
				INC(j)
			END;
			INC(i)
		END;
		class[7] := 1;
		piecemax[7] := 0+d*2+d*d*0;
		
		i := 0;
		WHILE i <= 0 DO j := 0;
			WHILE j <= 0 DO k := 0;
				WHILE k <= 2 DO
					p[8][i+d*(j+d*k)] := TRUE; INC(k)
				END;
				INC(j)
			END;
			INC(i)
		END;
		class[8] := 1;
     piecemax[8] := 0+d*0+d*d*2;
		
		i := 0;
		WHILE i <= 1 DO j := 0;
			WHILE j <= 1 DO k := 0;
				WHILE k <= 0 DO
					p[9][i+d*(j+d*k)] := TRUE; INC(k)
				END;
				INC(j)
			END;
			INC(i)
		END;
		class[9] := 2;
		piecemax[9] := 1+d*1+d*d*0;
		
		i := 0;
		WHILE i <= 1 DO j := 0;
			WHILE j <= 0 DO k := 0;
				WHILE k <= 1 DO
					p[10][i+d*(j+d*k)] := TRUE; INC(k)
				END;
				INC(j)
			END;
			INC(i)
		END;
		class[10] := 2;
		piecemax[10] := 1+d*0+d*d*1;
		
		i := 0;
		WHILE i <= 0 DO j := 0;
			WHILE j <= 1 DO k := 0;
				WHILE k <= 1 DO
					p[11][i+d*(j+d*k)] := TRUE; INC(k)
				END;
				INC(j)
			END;
			INC(i)
		END;
		class[11] := 2;
		piecemax[11] := 0+d*1+d*d*1;
		
		i := 0;
		WHILE i <= 1 DO j := 0;
			WHILE j <= 1 DO k := 0;
				WHILE k <= 1 DO
					p[12][i+d*(j+d*k)] := TRUE; INC(k)
				END;
				INC(j)
			END;
			INC(i)
		END;
		class[12] := 3;
		piecemax[12] := 1+d*1+d*d*1;
		
		piececount[0] := 13;
		piececount[1] := 3;
		piececount[2] := 1;
		piececount[3] := 1;
		m := 1+d*(1+d*1);
		kount := 0;
		IF Fit(0, m) THEN n := Place(0, m)
		ELSE Out.String("Error1 in Puzzle"); Out.Ln
		END;
		IF ~ Trial(n) THEN Out.String("Error2 in Puzzle."); Out.Ln
		ELSIF kount # 2005 THEN Out.String("Error3 in Puzzle."); Out.Ln
		END
	END Puzzle;


   (* Sorts an array using quicksort *)

	PROCEDURE Initarr();
		VAR i, temp: LONGINT;
	BEGIN
		Initrand();
		biggest := 0; littlest := 0; i := 1;
		WHILE i <= sortelements DO
			temp := Rand();
			sortlist[i] := temp - (temp DIV 100000)*100000 - 50000;
			IF sortlist[i] > biggest THEN biggest := sortlist[i]
			ELSIF sortlist[i] < littlest THEN littlest := sortlist[i]
			END ;
			INC(i)
		END
	END Initarr;

	PROCEDURE Quicksort(VAR a: ARRAY OF LONGINT; l,r: LONGINT);
  (* quicksort the array A from start to finish *)
		VAR i,j,x,w: LONGINT;
	BEGIN
		i:=l; j:=r;
		x:=a[(l+r) DIV 2];
		REPEAT
			WHILE a[i]<x DO i := i+1 END;
			WHILE x<a[j] DO j := j-1 END;
			IF i<=j THEN
				w := a[i];
				a[i] := a[j];
				a[j] := w;
				i := i+1;    j := j-1
			END ;
		UNTIL i > j;
		IF l<j THEN Quicksort(a,l,j) END;
		IF i<r THEN Quicksort(a,i,r) END
	END Quicksort;

	PROCEDURE* Quick ();
	BEGIN
     Initarr();
     Quicksort(sortlist,1,sortelements);
     IF (sortlist[1] # littlest) OR (sortlist[sortelements] # biggest) THEN  Out.String( " Error in Quick."); Out.Ln END ;
	END Quick;


    (* Sorts an array using bubblesort *)

	PROCEDURE bInitarr();
		VAR i, temp: LONGINT;
	BEGIN
		Initrand();
		biggest := 0; littlest := 0; i := 1;
		WHILE i <= srtelements DO
			temp := Rand();
			sortlist[i] := temp - (temp DIV 100000)*100000 - 50000;
			IF sortlist[i] > biggest THEN biggest := sortlist[i]
			ELSIF sortlist[i] < littlest THEN littlest := sortlist[i]
			END ;
			INC(i)
		END
	END bInitarr;

	PROCEDURE* Bubble();
		VAR i, j: LONGINT;
	BEGIN
		bInitarr();
		top:=srtelements;
		WHILE top>1 DO
			i:=1;
			WHILE i<top DO
				IF sortlist[i] > sortlist[i+1] THEN
					j := sortlist[i];
					sortlist[i] := sortlist[i+1];
					sortlist[i+1] := j;
				END ;
				i:=i+1;
			END;
			top:=top-1;
		END;
		IF (sortlist[1] # littlest) OR (sortlist[srtelements] # biggest) THEN Out.String("Error3 in Bubble."); Out.Ln END ;
	END Bubble;


    (* Sorts an array using treesort *)

	PROCEDURE tInitarr();
		VAR i, temp: LONGINT;
	BEGIN
		Initrand();
		biggest := 0; littlest := 0; i := 1;
		WHILE i <= sortelements DO
			temp := Rand();
			sortlist[i] := temp - (temp DIV 100000)*100000 - 50000;
			IF sortlist[i] > biggest THEN biggest := sortlist[i]
			ELSIF sortlist[i] < littlest THEN littlest := sortlist[i]
			END ;
			INC(i)
		END
	END tInitarr;

	PROCEDURE CreateNode (VAR t: node; n: LONGINT);
	BEGIN
    	NEW(t);
		t.left := NIL; t.right := NIL;
		t.val := n
	END CreateNode;


	PROCEDURE Insert(n: LONGINT; t: node);
	(* insert n into tree *)
	BEGIN
		IF n > t.val THEN
			IF t.left = NIL THEN CreateNode(t.left,n)
			ELSE Insert(n,t.left)
			END
		ELSIF n < t.val THEN
			IF t.right = NIL THEN CreateNode(t.right,n)
			ELSE Insert(n,t.right)
			END
		END
	END Insert;

	PROCEDURE Checktree(p: node): BOOLEAN;
    (* check by inorder traversal *)
		VAR result: BOOLEAN;
	BEGIN
		result := TRUE;
		IF p.left # NIL THEN
			IF p.left.val <= p.val THEN result := FALSE;
			ELSE result := Checktree(p.left) & result
			END
		END ;
		IF  p.right # NIL THEN
			IF p.right.val >= p.val THEN result := FALSE;
			ELSE result := Checktree(p.right) & result
			END
		END;
		RETURN result
	END Checktree;

	PROCEDURE* Trees();
		VAR i: LONGINT;
	BEGIN
		tInitarr();
		NEW(tree);
		tree.left := NIL; tree.right:=NIL; tree.val:=sortlist[1];
		i := 2;
     WHILE i <= sortelements DO
     	Insert(sortlist[i],tree);
     	INC(i)
     END;
		IF ~ Checktree(tree) THEN Out.String(" Error in Tree."); Out.Ln END;
	END Trees;

	PROCEDURE Cos (x: REAL): REAL;
	(* computes cos of x (x in radians) by an expansion *)
		VAR i, factor: LONGINT;
			result,power: REAL;
	BEGIN
		result := 1.0; factor := 1;  power := x; i := 2;
		WHILE i <= 10 DO
			factor := factor * i;  power := power*x;
			IF i MOD 2 = 0 THEN
				IF i MOD 4 = 0 THEN result := result + power/factor
				ELSE result := result - power/factor
				END
			END;
			INC(i)
		END ;
		RETURN result
	END Cos;
	
	PROCEDURE Min0( arg1, arg2: LONGINT): LONGINT;
	BEGIN
		IF arg1 < arg2 THEN RETURN arg1
		ELSE RETURN arg2
		END
	END Min0;

	PROCEDURE Uniform11(iy: LONGINT; yfl: REAL);
	BEGIN
		iy := (4855*iy + 1731) MOD 8192;
		yfl := iy/8192.0;
	END Uniform11;

	PROCEDURE Exptab(n: LONGINT; VAR e: c2array);
		VAR theta, divisor: REAL; h: ARRAY 26 OF REAL;
			i, j, k, l, m: LONGINT;
	BEGIN
		theta := 3.1415926536;
		divisor := 4.0; i:=1;
		WHILE i <= 25 DO
			h[i] := 1/(2*Cos( theta/divisor ));
			divisor := divisor + divisor;
			INC(i)
		END;
		m := n DIV 2 ;
		l := m DIV 2 ;
		j := 1 ;
		e[1].rp := 1.0 ;
		e[1].ip := 0.0;
		e[l+1].rp := 0.0;
		e[l+1].ip := 1.0 ;
		e[m+1].rp := -1.0 ;
		e[m+1].ip := 0.0 ;
		REPEAT
			i := l DIV 2 ;
			k := i ;
			REPEAT
				e[k+1].rp := h[j]*(e[k+i+1].rp+e[k-i+1].rp) ;
				e[k+1].ip := h[j]*(e[k+i+1].ip+e[k-i+1].ip) ;
				k := k+l ;
			UNTIL ( k > m );
			j := Min0( j+1, 25);
			l := i ;
		UNTIL ( l <= 1 );
	END Exptab;

	PROCEDURE Fft( n: LONGINT; VAR z, w: carray; VAR e: c2array; sqrinv: REAL);
		VAR i, j, k, l, m, index: LONGINT; h: REAL;
	BEGIN
		m := n DIV 2 ;
		l := 1 ;
		REPEAT
			k := 0 ;
			j := l ;
			i := 1 ;
			REPEAT
				REPEAT
					w[i+k].rp := z[i].rp+z[m+i].rp ;
					w[i+k].ip := z[i].ip+z[m+i].ip ;
					h := e[k+1].rp*(z[i].rp-z[i+m].rp);
					w[i+j].rp := h-e[k+1].ip*(z[i].ip-z[i+m].ip) ;
					h := e[k+1].rp*(z[i].ip-z[i+m].ip);
					w[i+j].ip := h+e[k+1].ip*(z[i].rp-z[i+m].rp) ;
					i := i+1 ;
				UNTIL ( i > j );
				k := j ;
				j := k+l ;
			UNTIL ( j > m );
			(*z := w ;*) index := 1;
			REPEAT
				z[index] := w[index];
				index := index+1;
			UNTIL ( index > n );
			l := l+l ;
		UNTIL ( l > m );
		i := 1;
		WHILE i <= n DO
			z[i].rp := sqrinv*z[i].rp ;
			z[i].ip := -sqrinv*z[i].ip;
			INC(i)
		END
	END Fft;
	
	PROCEDURE* Oscar ();
		VAR i: LONGINT;
	BEGIN
		Exptab(fftsize,e) ;
		seed := 5767 ; i := 1;
		WHILE i <= fftsize DO
			Uniform11( seed, zr );
			Uniform11( seed, zi );
			z[i].rp := 20.0*zr - 10.0;
			z[i].ip := 20.0*zi - 10.0;
			INC(i)
		END ;
		i := 1;
		WHILE i <= 20 DO Fft(fftsize,z,w,e,0.0625); INC(i) END
	END Oscar;
	
	PROCEDURE Time(s: ARRAY OF CHAR; p: Proc; base, fbase: REAL);
		VAR timer, iter: LONGINT; 
	BEGIN
		Out.String(s);
		timer := Getclock();
		iter := 10; WHILE iter > 0 DO p; DEC(iter) END ;
		timer := (Getclock()-timer+5) DIV 10;
		Out.Int(timer, 8); Out.Ln;
		fixed := fixed + timer*base;
		floated := floated + timer*fbase
	END Time;

	PROCEDURE main*;
	BEGIN
		fixed := 0.0;  floated := 0.0;
		Time("Perm ", Perm, permbase, permbase);
		Time("Towers ", Towers, towersbase, towersbase);
		Time("Queens ", Queens, queensbase, queensbase);
		Time("Intmm ", Intmm, intmmbase, intmmbase);
		Time("Mm ", Mm, mmbase, fpmmbase);
		Time("Puzzle ", Puzzle, puzzlebase, puzzlebase);
		Time("Quick ", Quick, quickbase, quickbase);
		Time("Bubble ", Bubble, bubblebase, bubblebase);
		Time("Tree ", Trees, treebase, treebase);
		Time("FFT ", Oscar, fftbase, fpfftbase);
		Out.String("Nonfloating point composite is "); Out.Int(ENTIER(fixed/10+0.5), 10); Out.Ln;
		Out.String("Floating point composite is "); Out.Int(ENTIER(floated/10+0.5), 10); Out.Ln;

	END main;
BEGIN
	main;
END HennessyV4.

