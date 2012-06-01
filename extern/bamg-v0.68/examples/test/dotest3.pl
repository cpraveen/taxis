#!/usr/local/bin/perl
# -- so option
# -----  clean ---
unlink <*.mesh>;
unlink <*.mtr>;
unlink <*.bb>;
unlink <YG_trace*>;
unlink 'PLOT';
unlink 'RESIDUALS';
unlink <err*.BB>;
unlink <output_bamg_*>;
##$f="10 +  1/(1+ 100**(sin(x*3)-y)) ";
#$f1 = "(10*x*x*x+y*y*y) + 10/(1+10**(10*((sin(5*x)-2*y)))) ";
#$f2=" sin(3*x)*cos(5*y)+ atan2(0.001,x*x+y*y-0.5)";
$ee= 0.05;
sub tanh {
   my $x = shift;
    if($x >20) {return 1;}
   if($x <-20) {return -1;}
   my $ex=exp($x);
   my $ex1=1.0/$ex;
   my $r = ($ex-$ex1)/($ex+$ex1);
   return $r;
};
sub f1 {
  my $x= shift; my $y=shift;
  return 3*tanh((($x)-$y)/$ee);
};
sub f2 {
  return 1;
};

print tanh(40)," ",tanh(-40)," ",tanh(19)," ",tanh(-19)," ",tanh(1), "\n";
$err=0.1;
$errg=0.01;
$nbiteration=20;
$bamg="../../bamg";
$quadoption="";
$bamgoption=" -AbsError -NoRescaling   -NbSmooth 0 -omega 1  -hmax 2 -H 0 -NbJacobi 2 -maxsubdiv 2 -hmin 0.0000005 -ratio 2 -nbv 100000 -v 4";
#$bamgoption=" -AbsError -NbJacobi 3  -ratio 2 -anisomax 30";
#$quadoption=" -2q -thetaquad 30 -coef 2";

#--------------------------
$suffixe=".mesh";
$iteration=0;
$GH="Gh$suffixe";
$TH="Th$iteration$suffixe";
$, = ' ';               # set output field separator
$\ = "\n";              # set output record separator
##  -------------------------------------------------------------
##  --- construction the Geometry file Gh.mesh 

##    8 points on circle of radius r 
open(GH,">$GH")  || die "Can't redirect stdout";
$Pi = 3.14159265358979;

$i8 = 80;
$r = 10;
$c0x = 0;
$c0y = 0;
print GH 'Dimension', 2;
print GH 'MaximalAngleOfCorner 46';

print GH 'Vertices';
print GH   $i8;

# vertex on circle  (counter clock wise)
for ($i = 0; $i < $i8; $i++) {
    $t = $i * $Pi * 2 / $i8;
    print GH $c0x + $r * (cos($t)), $c0y + $r * sin($t), 5;
}

print GH 'Edges',  $i8+1;

print GH 1,5,10;
# edge on circle 
$k =  1;
$j = $i8 - 1;
# previous points
for ($i = 0; $i < $i8; $j = $i++) {
    print GH $k + $j, $k + $i, 5;
}
# previous, current vertex

#   one subdomain, region on left side of the wing
#   because clock wise sens.
print GH 'SubDomain', 2;
print GH 2, 1, 1, 0;
print GH 2, 1, -1, 1;
close GH;
##  -------------- END construct of the geom 
## 
# -- make the DATA file for the mesh to also  save the arguments 
open(BAMG,">DATA_bamg")  || die "Can't open  DATA_bamg";
print BAMG "$quadoption $bamgoption  -g $GH -o $TH  -v 9";
close BAMG;

## constructio the inital  mesh 
!system("$bamg  >>output_bamg_$iteration") || die "Error in bamg construction of initial  mesh $Th";

##  the adpatation loop 
while ($iteration<$nbiteration) {
     $hmin=1;
     $hmax=2;
     $aniso=1;
     open(BAMG_OUTPUT,"<output_bamg_$iteration") || die " Erreur open output_bamg_$iteration";
     
     while (<BAMG_OUTPUT>)
       { 
	if(/output:  Hmin =/) {
	     ($i1,$i2,$i3,$i4,$hmin,$i6,$i7,$hmax,$i9,$i10,$i11,$i12,$i12,$aniso ) = split(/[\ \t\n]+/);
	   }
       }
      close BAMG_OUTPUT;

    $BB="$iteration.BB";   
    $BBa="$iteration.1.bb"; 
    $BBb="$iteration.2.bb";
    $eBB="err_$iteration.BB"; 
    
##  construction of the solution  
    $errsol1=0;
    $errsol2=0;
    open (TH,"<$TH") ||  die "Can't open  $TH";
    open (BB,">$BB") ||  die "Can't open  $BB";
    open (BBa,">$BBa") ||  die "Can't open  $BBa";
    open (BBb,">$BBb") ||  die "Can't open  $BBb";
    open (eBB,">$eBB") ||  die "Can't open  $eBB";
    open (PLOT,">PLOT") || die "Can't open PLOT";
    open (POINT,">$iteration.points") ||die "Can't open points";
    open (FACE,">$iteration.faces") ||die "Can't open points";
    while (<TH>) {
	if(/^Vertices$/) {
	    $nbv=<TH>;
	    chop($nbv);
	    print BB "2 2 1 1 $nbv 2";
	    print BBa "2 1 $nbv 2";
	    print BBb "2 1 $nbv 2";
            print POINT "$nbv";
	    for ($i=1;$i<=$nbv;$i++) {
		($x,$y,$ref)=split(/[\ \t\n]+/, <TH>);
		$f1xy=f1($x,$y);
		$f2xy=f2($x,$y);

		$xx[$i]=$x;
		$yy[$i]=$y;
		$ff1[$i]=$f1xy;
		$ff2[$i]=$f2xy;


		print BB $f1xy,$f2xy;
		print BBa $f1xy;
		print BBb $f2xy;
		print POINT $x,$y,$f1xy,"0"
	    };
	};
	if(/^Triangles$/) {
	    $nbt=<TH>;
	    chop($nbt);

	    print eBB " 2 2  1 1  $nbt 1";
            print FACE $nbt;
	    for ($i=1;$i<=$nbt;$i++) {
		($i0,$i1,$i2,$ref)=split(/[\ \t\n]+/, <TH>);
		print PLOT "$xx[$i0] $yy[$i0] $ff[$i0]";
		print PLOT "$xx[$i1] $yy[$i1] $ff[$i1]";
		print PLOT "$xx[$i2] $yy[$i2] $ff[$i2]";
		print PLOT "$xx[$i0] $yy[$i0] $ff[$i0]";
		print PLOT "";

		$x   = ($xx[$i0]+$xx[$i1]+$xx[$i2])/3;
		$y   = ($yy[$i0]+$yy[$i1]+$yy[$i2])/3;
		$fm1  = ($ff1[$i0]+$ff1[$i1]+$ff1[$i2])/3;
		$fm2  = ($ff2[$i0]+$ff2[$i1]+$ff2[$i2])/3;
		$fxy1 = f1($x,$y);
		$fxy2 = f2($x,$y); 
		$vv1=($fm1-$fxy1);
		$vv1= ($vv1<0)?-$vv1:$vv1;
 		$vv2=($fm2-$fxy2);
		$vv2= ($vv2<0)?-$vv2:$vv2;

		$errsol1 = ($errsol1>$vv1) ? $errsol1 : $vv1;
		$errsol2 = ($errsol2>$vv2) ? $errsol2 : $vv2;
		print eBB "$vv1 $vv2";
                print FACE 3,$i0,$i1,$i2," 0 0 0 0 ";
	    };
	};

    };
    close POINT;
    close FACE;
    close TH;
    close BB;
    close BBa;
    close BBb;
    close eBB;
    close PLOT;
    print "$iteration, err=$errsol1 $errsol2, nbt=$nbt nbv=$nbv hmin=$hmin hmax=$hmax aniso=$aniso";
    open(RESIDUALS,">>RESIDUALS");
    print RESIDUALS " $iteration $nbt $nbv $errsol1  $errsol2 $hmin $hmax $aniso";
    close RESIDUALS;
##  -----------------------
    
    $MTR="M$iteration.mtr";

    $iteration++;
    $BTH=$TH;
    $TH="Th$iteration$suffixe";

    open(BAMG,">DATA_bamg")  || die "Can't open  DATA_bamg";
    print BAMG "$quadoption  $bamgoption  -MBB $BB -errg $errg -err $err   -b $BTH -o $TH   -oM $MTR ";
    close BAMG;
    !system("$bamg  >>output_bamg_$iteration ") ||    die "Error in bamg construction of adapted $iteration  mesh $Th";
}
print "Normal End\n";
