// ----------------------------------------------------------------
// 
//   4190.308 Computer Architecture (Fall 2022)
// 
//   Project #2: SFP16 (16-bit floating point) Adder
// 
//   October 4, 2022
// 
//   Seongyeop Jeong (seongyeop.jeong@snu.ac.kr)
//   Jaehoon Shim (mattjs@snu.ac.kr)
//   IlKueon Kang (kangilkueon@snu.ac.kr)
//   Wookje Han (gksdnrwp@snu.ac.kr)
//   Jinsol Park (jinsolpark@snu.ac.kr)
//   Systems Software & Architecture Laboratory
//   Dept. of Computer Science and Engineering
//   Seoul National University
// 
// ----------------------------------------------------------------

typedef unsigned short SFP16;

/* Add two SFP16-type numbers and return the result */

SFP16 fpadd(SFP16 x, SFP16 y)
{
  /* TODO */
  SFP16 res = 0;

  // Step 0: define variable
  SFP16 sign = 0, expo = 0, m = 0, g = 0, r = 0, s = 0, tmp = 1;

  // Step 1: swap and initialization
  if (x%(1<<15) < y%(1<<15)) {
    SFP16 tmp = x;
    x = y;
    y = tmp;
  }
  SFP16 sx = x>>15, sy = y>>15;
  SFP16 ex = ((x-(sx<<15))>>8), ey = ((y-(sy<<15))>>8);
  SFP16 mx = x-(sx<<15)-(ex<<8), my = y-(sy<<15)-(ey<<8);
  // check NaN - find special values
  SFP16 NaN = (SFP16) 0x7f01;
  if (ex == 127 && mx != 0) return NaN;
  if (ey == 127 && my != 0) return NaN;
  // check inf - find special values
  if (ex == 127 && mx == 0) {
    if (ey == 127 && my == 0 && sx != sy) return NaN;
    return x;
  }
  else if (ey == 127 && my == 0) return y;
  // check normal or denormal
  if (ex != 0) mx += 1<<8;
  if (ey != 0) my += 1<<8;
  if (ex == 0 && ey == 0) tmp = 0;
  sign = sx;
  expo = ex;
  
  // Step 2: extend my
  if (ex == 0) ex++;
  if (ey == 0) ey++;  
  for (SFP16 i = 0; i < ex-ey; i++) {
    s = r | s;
    r = g;
    g = my % 2;
    my >>= 1;
  }
  
  // Step 3: addition or subtraction
  if (sx == sy) m = mx+my;
  else {
    mx <<= 3;
    my = (my<<3)+(g<<2)+(r<<1)+s;
    m = mx-my;
    s = m%2;
    r = (m%4-s)/2;
    g = (m%8-s-r)/4;
    m >>= 3;
  }

  // Step 4: normalize m
  // Step 5: round-to-even
  // Step 6: repeating step 5, 6
  SFP16 flag2 = 1;
  while (flag2 != 0) {
    // normalize
    if (m >= 512) { // shift right
      while (m >= 512) {
        s = r | s;
        r = g;
        g = m % 2;
        m >>= 1;
        expo++;
      }
      flag2 = 0;
    }
    else if (m < 256) { // shift left
      m = (m<<3)+(g<<2)+(r<<1)+s;
      while (m < 2048 && expo > 1) {
        m <<= 1;
        expo--;
      }
      if (m < 2048 && expo == 1) expo--;
      s = m%2;
      r = (m%4-s)/2;
      g = (m%8-s-r)/4;
      m >>= 3;
      flag2 = 0;
    }
    else if (flag2 == 2) break;
    else flag2 = 0;

    // rounding
    if (g == 1 && (r == 1 || s == 1)) {
      m += 1;
      flag2 = 2;
    }
    else if (m%2 == 1 && g == 1) {
      m += 1;
      flag2 = 2;
    }
  }

  // check answer is inf
  if (expo == 127 && sign == 0) return 0x7f00;
  if (expo == 127 && sign == 1) return 0xff00;

  // Step 7: encode the result
  res = sign<<7;
  res += expo;
  res <<= 8;
  if (tmp == 1) res += (m%(1<<8));
  else res += m;
  return res;
}
