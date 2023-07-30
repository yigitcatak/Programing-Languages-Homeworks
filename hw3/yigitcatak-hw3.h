#ifndef __YIGITCATAK_HW3_H
#define __YIGITCATAK_HW3_H

enum dtype {dint, dfloat, dstring};
typedef struct DataNode
{
   char *value;
   enum dtype dtype;
   unsigned int lineNum;
   struct DataNode * parent1;
   struct DataNode * parent2;
} DataNode;

#endif