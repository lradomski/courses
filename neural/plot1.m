x=linspace(-2,2,50);
y=linspace(-2,2,50);
[xx,yy]=meshgrid(x,y);
mask = xx + yy > 0;
xxm = xx.*mask;
yym = yy.*mask;
mesh(xxm, yym, xxm+yym);

