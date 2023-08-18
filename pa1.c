//---------------------------------------------------------------
//
//  4190.308 Computer Architecture (Fall 2022)
//
//  Project #1:
//
//  September 6, 2022
//
//  Seongyeop Jeong (seongyeop.jeong@snu.ac.kr)
//  Jaehoon Shim (mattjs@snu.ac.kr)
//  IlKueon Kang (kangilkueon@snu.ac.kr)
//  Wookje Han (gksdnrwp@snu.ac.kr)
//  Jinsol Park (jinsolpark@snu.ac.kr)
//  Systems Software & Architecture Laboratory
//  Dept. of Computer Science and Engineering
//  Seoul National University
//
//---------------------------------------------------------------

typedef unsigned char u8;

// Functions for all of the encoding 
void encoding(int* pre, int len, u8* result, int* cnt, int value) {
  if (len >= 8-*pre) {
    *(result+*cnt) <<= 8-*pre;
    *(result+*cnt) += (value >> (*pre+len-8));
    (*cnt)++;
    *pre = len - (8-*pre);
    if (*pre == 0) return;
    value = (value << (8-*pre)) >> (8-*pre);
    *(result+*cnt) <<= *pre;
    *(result+*cnt) += value;
  }
  else {
    *(result+*cnt) <<= len;
    *(result+*cnt) += value;
    *pre += len;
  }
  return;
}

/* TODO: Implement this function */
int encode(const u8* src, int width, int height, u8* result) {

  // Using this variables for encoding
  int cnt = 0, pre = 0;

  if (width == 0 || height == 0) return 0;
  for (int i = 0; i < height; i++) {
    u8 mini = 255, maxi = 0, n;

    // Check number of src
    for (int j = 0; j < width; j++) {
      int now = i*width+j;
      u8 tmp;

      // Calculate average of three neighboring cells
      if (i == 0 && j == 0) tmp = 0;
      else if (i == 0) tmp = *(src+now-1);
      else if (j == 0) tmp = *(src+now-width);
      else tmp = (*(src+now-1)+*(src+now-width)+*(src+now-width-1)) / 3;

      // Computing difference
      tmp = (*(src+now)+256-tmp) % 256;

      // Find min (base(i)) & max value at same row
      if (mini > tmp) mini = tmp;
      if (maxi < tmp) maxi = tmp;
    }

    // Find n(i) value
    u8 delta = maxi-mini;
    if (delta == 0) n = 0;
    else if (delta == 1) n = 1;
    else if (delta < 4) n = 2;
    else if (delta < 8) n = 3;
    else if (delta < 16) n = 4;
    else if (delta < 32) n = 5;
    else if (delta < 64) n = 6;
    else if (delta < 128) n = 7;
    else n = 8;

    // Encode and Calculate for base(i) & n(i)
    encoding(&pre, 8, result, &cnt, mini);
    encoding(&pre, 4, result, &cnt, n);
    if (n == 0) continue;

    // Encode and Calculate for each Cells
    for (int j = 0; j < width; j++) {
      int now = i*width+j;
      u8 tmp;
      if (i == 0 && j == 0) tmp = 0;
      else if (i == 0) tmp = *(src+now-1);
      else if (j == 0) tmp = *(src+now-width);
      else tmp = (*(src+now-1)+*(src+now-width)+*(src+now-width-1)) / 3;
      tmp = (*(src+now)+256-tmp) % 256;
      tmp -= mini;
      encoding(&pre, n, result, &cnt, tmp);
    }
  }

  // Final padding for using 8-bits
  if (pre != 0) encoding(&pre, 8-pre, result, &cnt, 0);

  return cnt;
}