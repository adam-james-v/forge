rotate_extrude(convexity=1, angle=360, $fa=12, $fn=0, $fs=2) {
  multmatrix([[1,0,0,80],[0,1,0,0],[0,0,1,0],[0,0,0,1]]) {
    square(size=[10, 20], center=true);
  }
}
