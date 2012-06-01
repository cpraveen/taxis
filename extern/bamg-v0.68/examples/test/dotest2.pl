#!/usr/local/bin/perl
# -- so option
#  loop with ftq file  for test 
# -----  clean ---
unlink <*.ftq>;
unlink <*.mtr>;
unlink <*.bb>;
unlink <YG_trace*>;
unlink 'PLOT';

##$f="10 +  1/(1+ 100**(sin(x*3)-y)) ";
#$f1 = "(10*x*x*x+y*y*y) + 10/(1+10**(10*((sin(5*x)-2*y)))) ";
#$f2=" sin(3*x)*cos(5*y)+ atan2(0.001,x*x+y*y-0.5)";
$f1 = " (10*x*x*x+y*y*y) +  atan2(0.001,(sin(5*y)-2*x))";
$f2 = " (10*y*y*y+x*x*x) +  atan2(0.001,(sin(5*x)-2*y))";
$err=0.05;
$errg=0.01;
$nbiteration=20;
$bamg="../../bamg-g";
$quadoption="";
$bamgoption="  -thetamax 60 -AbsError -NbJacobi 2  -NbSmooth 5 -hmax 0.5  -hmin 0.0000005 -ratio 0  -nbv 100000";
#$bamgoption=" -AbsError -NbJacobi 3  -ratio 2 -anisomax 30";
#$quadoption=" -2q -thetaquad 30 -coef 2";

# ---  change x in $x and y in $y 
$_=$f1;
s/x/\$x/g;
s/y/\$y/g;
$f1="$_;";
$_=$f2;
s/x/\$x/g;
s/y/\$y/g;
$f2="$_;";

print "The function = f1(x,y) = $f1 and  f2(x,y) = $f2  \n";

#--------------------------
$suffixe=".ftq";
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

$i8 = 8;
$r = 1;
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
print BAMG "$quadoption $bamgoption  -g $GH -oftq  $TH  -v 9";
close(BAMG);

## constructio the inital  mesh 
!system($bamg) || die "Error in bamg construction of initial  mesh $Th";

##  the adpatation loop 
while ($iteration<$nbiteration) {

    

    $BB="$iteration.bb";   
    $BBa="$iteration.1.bb"; 
    $BBb="$iteration.2.bb";
    
##  construction of the solution  
    $errsol=0;
    open (TH,"<$TH") ||  die "Can't open  $TH";
    open (BB,">$BB") ||  die "Can't open  $BB";
    open (BBa,">$BBa") ||  die "Can't open  $BB";
    open (BBb,">$BBb") ||  die "Can't open  $BB";
    open (PLOT,">PLOT") || die "Can't open PLOT";

    ($nbv,$nbe,$nbt,$nbq)=split(/[\ \t\n]+/, <TH>);
#    chop($nbv);
#    chop($nbt);
    if ($nbq) {die " Some quad ";} 
    print " number of vertices $nbv , number of triangles $nbt ";
    print " ---------------------------------------------------";
    print BB "2 2 $nbv 2";
    print BBa "2 1 $nbv 2";
    print BBb "2 1 $nbv 2";

    for ($i=1;$i<=$nbt;$i++) {
	($kk,$ti0[$i],$ti1[$i],$ti2[$i],$tref[$i])=split(/[\ \t\n]+/, <TH>);
    };

    for ($i=1;$i<=$nbv;$i++) {
	($x,$y,$ref)=split(/[\ \t\n]+/, <TH>);
	$f1xy=eval $f1;
	$f2xy=eval $f2;
	
	$xx[$i]=$x;
	$yy[$i]=$y;
	$ff[$i]=$f1xy;
	
	
	print BB $f1xy,$f2xy;
	print BBa $f1xy;
	print BBb $f2xy;
    };

    for ($i=1;$i<=$nbt;$i++) {
	($i0,$i1,$i2,$ref)=($ti0[$i],$ti1[$i],$ti2[$i],$tref[$i]);
	print PLOT "$xx[$i0] $yy[$i0] $ff[$i0]";
	print PLOT "$xx[$i1] $yy[$i1] $ff[$i1]";
	print PLOT "$xx[$i2] $yy[$i2] $ff[$i2]";
	print PLOT "$xx[$i0] $yy[$i0] $ff[$i0]";
	print PLOT "";
	
	$x   = ($xx[$i0]+$xx[$i1]+$xx[$i2])/3;
	$y   = ($yy[$i0]+$yy[$i1]+$yy[$i2])/3;
	$fm  = ($ff[$i0]+$ff[$i1]+$ff[$i2])/3;
	$fxy = eval $f1;
	$vv=($fm-$fxy);
	$vv= ($vv<0)?-$vv:$vv;
	#	print " $i0 $i1 $i2 $xx[$i0] $xx[$i1] $xx[$i2] ";
	#	print " $i $x $y $fm $fxy err= $errsol diff=$vv";
	$errsol = ($errsol>$vv) ? $errsol : $vv;
    };

    close TH;
    close BB;
    close BBa;
    close BBb;
    close PLOT;
    print " ---------------------------------------------\n";
    print "\n\n Iteration $iteration\n Erreur L_infini = $errsol \n\n";
    print " ---------------------------------------------\n";

##  -----------------------
    
    $MTR="M$iteration.mtr";

    $iteration++;
    $BTH=$TH;
    $TH="Th$iteration$suffixe";

    open(BAMG,">DATA_bamg")  || die "Can't open  DATA_bamg";
    print BAMG "$quadoption  $bamgoption  -Mbb $BB -errg $errg -err $err   -b $BTH -oftq $TH  -v 9 -oM $MTR ";
    close(BAMG);
    !system($bamg) ||    die "Error in bamg construction of adapted $iteration  mesh $Th";
}

print "Normal End\n";
