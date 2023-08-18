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

#include <stdio.h>

typedef unsigned short SFP16;

#define NAN ((SFP16) 0x7f01)

#define N 4


/* norm + norm */
SFP16 test1_x[N] =  {0x17f2, 0x8b00, 0x8201, 0x0afe};
SFP16 test1_y[N] =  {0x154f, 0x0100, 0x0202, 0x0288};
SFP16 ans1[N] =     {0x1823, 0x8b00, 0x0002, 0x0b00};

/* norm + denorm */
SFP16 test2_x[N] =  {0x03f3, 0x05e1, 0x0901, 0x04f2};
SFP16 test2_y[N] =  {0x00ff, 0x80f3, 0x0080, 0x0003};
SFP16 ans2[N] =     {0x0419, 0x05d2, 0x0902, 0x04f2};

/* denorm + denorm */
SFP16 test3_x[N] =  {0x0024, 0x0005, 0x80fe, 0x00ff};
SFP16 test3_y[N] =  {0x0037, 0x8007, 0x8007, 0x0001};
SFP16 ans3[N] =     {0x005b, 0x8002, 0x8105, 0x0100};

/* +INF, -INF, NAN, -NAN, etc */
SFP16 test4_x[N] =  {0x7f00, 0x7f00, 0x7f00,    NAN};
SFP16 test4_y[N] =  {   NAN, 0xff00, 0x0030, 0x0001};
SFP16 ans4[N] =     {   NAN,    NAN, 0x7f00,    NAN};

SFP16 *tc_x[] =     { test1_x, test2_x, test3_x, test4_x };
SFP16 *tc_y[] =     { test1_y, test2_y, test3_y, test4_y };
SFP16 *tc_ans[] =   {    ans1,    ans2,    ans3,    ans4 };
char *tc_msg[] =    { "-------- NORM + NORM --------\n",
                      "-------- NORM + DENORM ------\n",
                      "------ DENORM + DENORM ------\n",
                      "--------- INF / NAN ---------\n", };

extern SFP16 fpadd(SFP16 x, SFP16 y);

SFP16 check(SFP16 a)
{
  SFP16 b = a & 0x7fff;

  /* We do not distinguish +0 and -0 */
  if (b == 0)
    return 0;
  
  /* We do not distinguish +NaN and -NaN */
  if (b == (SFP16) 0x7f01)
    return NAN;

  return a;
}

int main(void) {
  int i, j;

  for (i = 0; i < sizeof(tc_x) / sizeof(SFP16 *); i++) {
    printf("%s", tc_msg[i]);
    for (j = 0; j < N; j++) {
      printf("test %d: %s\n", j,
             (check(fpadd(tc_x[i][j], tc_y[i][j])) == check(tc_ans[i][j]))?
             "Correct" : "Wrong");
    }
  }
  return 0;
}
