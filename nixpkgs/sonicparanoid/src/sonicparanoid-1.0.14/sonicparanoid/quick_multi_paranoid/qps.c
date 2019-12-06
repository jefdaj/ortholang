/* Copyright (c) 2008 
   Taekyung Kim, Jinha Kim, Minyoung Son, Hyeonseung Im, Sungwoo Park    
   Programming Language Laboratory, POSTECH
   gla@postech.ac.kr */

#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<unistd.h>
#include "qp.h"

#define MAX_STRING_LENGTH 256
#define MAX_NUM_SPECIES 120

#define TAIL_ON 1
#define TAIL_OFF 0


//int getSpeciesList(char* strConfig, char** strSpeciesList);
//int getFileList(char* strConfig, char** strSpeciesList);
char** getSpeciesList(char* strConfig, int* numSpecies);
char** getFileList(char* strConfig, int* numFiles);
int getNumLine(char* strDataName);
int getSize(char* strDataName);
int tokenizer(FILE *input, char* buf);
int file_length(char* fileName);
int get_numLines(char* fileName);

//-------------------------------------------------------
main(int argc, char** argv){
  char** strFileList = NULL;
  char** strDumpList = NULL;
  int i;
  int numFiles;  
  int totalSize, totalLines;

  strFileList = getFileList(FILE_CONFIG, &numFiles);
/*
   for (i = 0; i < numFiles; i++){
     printf("%s\n", strFileList[i]);
   }
  */
  totalSize = 0;
  totalLines = 0;
  for(i = 0; i < numFiles; i++){
    totalSize += file_length(strFileList[i]);
    totalLines += get_numLines(strFileList[i]);
  }

  // printf("\n%s, \n", argv[1]);
  printf("Dataset size in bytes: %d\nNumber of entries in the dataset: %d\n", totalSize, totalLines);

  // free
  for (i = 0; i < numFiles; i++){
    free (strFileList[i]);
  }
  free (strFileList);
}


//-------------------------------------------------------
char** getSpeciesList(char* strConfig, int* numSpecies){
  char** strSpeciesList = NULL;
  int tempNumSpecies = 0;
  FILE* input;
  char buf[MAX_STRING_LENGTH];
  int iBufLength;
  int i;
  
  input = fopen(strConfig, "r"); 

  if (input == NULL){
    printf("Fail to load %s\n", strConfig);
    exit(0);
  }

  // get the num of species
  tempNumSpecies = 0;
  while(tokenizer(input, buf)){
    tempNumSpecies++;
  }
  rewind(input);

  // get species list
  strSpeciesList = (char**) malloc (sizeof(char*) * tempNumSpecies);
  for (i = 0; i < tempNumSpecies; i++){
    strSpeciesList[i] = (char*) malloc (sizeof(char) * MAX_STRING_LENGTH);
  }

  for(i = 0; i< tempNumSpecies; i++){
    iBufLength = tokenizer(input, buf);
    strncpy(strSpeciesList[i], buf, iBufLength);
  }

  *numSpecies = tempNumSpecies;

  fclose(input);
  return strSpeciesList;
}

//-------------------------------------------------------
char** getFileList(char* strConfig, int* numFiles){
  int i, j, k;
  char** strSpeciesList = NULL;
  char** strFileList = NULL;
  int numSpecies = 0;
  int tempNumFiles = 0;

  // get species list
  strSpeciesList = getSpeciesList(strConfig, &numSpecies);

  // get file list
  tempNumFiles = numSpecies * (numSpecies - 1)/ 2;

  strFileList = (char**) malloc (sizeof(char*) * tempNumFiles);
  for (i = 0; i < tempNumFiles; i++){
    strFileList[i] = (char*) malloc (sizeof(char) * MAX_STRING_LENGTH);
  }

  k = 0;
  for (i = 0; i < numSpecies; i++){
    for (j = 0; j < numSpecies; j++){
      if (i<j){
        strcpy(strFileList[k], FILE_DIRECTORY);
        strcat(strFileList[k], FILE_PREFIX);
        strcat(strFileList[k], strSpeciesList[i]);
        strcat(strFileList[k], FILE_SEPARATOR);
        strcat(strFileList[k], strSpeciesList[j]);
//		strcat(strFileList[k], INTERMEDIATE_DATAFILE_SUFFIX);
	if( access(strFileList[k],R_OK) != 0)
	  {
	    strcpy(strFileList[k], FILE_DIRECTORY);
	    strcat(strFileList[k], FILE_PREFIX);
	    strcat(strFileList[k], strSpeciesList[j]);
	    strcat(strFileList[k], FILE_SEPARATOR);
	    strcat(strFileList[k], strSpeciesList[i]);
	  }
        k++;
      }
    }
  }

  *numFiles = tempNumFiles;

  // free
  for (i = 0; i < numSpecies; i++){
    free (strSpeciesList[i]);
  }
  free (strSpeciesList);

  return strFileList;
}


//-------------------------------------------------------
int tokenizer(FILE *input, char* buf){
  int ch;
  int buf_p = 0;

  ch = getc(input); 

  // remove white space
  while(ch == ' ' || ch == '\t' || ch == '\n') {
    ch = getc( input );
  }       

  // remove comment
  while (ch == '#'){
    while(ch != '\n') {
      ch = getc( input );
    }
    while(ch == ' ' || ch == '\t' || ch == '\n') {
      ch = getc( input );
    }       
  }

  // if EOF
  if (ch == EOF)
    return 0;

  // tokenize
  while(ch != ' ' && ch != '\t' && ch != '\n') {
    //printf("%c", ch );
    buf[buf_p] = ch;
    buf_p++;
    ch = getc( input );
  }       

  // make the last char be null
  buf[buf_p]='\0';

  return buf_p;
}

//-------------------------------------------------------
/*
 * returns the length of a file (in bytes)
 */
int file_length(char* fileName){
  int pos;
  int end;
  int temp;
  FILE* input;
// data file
  input = fopen(fileName, "r"); 

  if (input == NULL){
    printf("Fail to load %s\n", fileName);
    exit(0);
  }

  pos = ftell (input);
  fseek (input, 0, SEEK_END);
  end = ftell (input);
  fseek (input, pos, SEEK_SET);

  fclose(input);
  
  temp = end;

// dumped file

  fileName = strcat(fileName, INTERMEDIATE_DATAFILE_SUFFIX);
  fileName[strlen(fileName)] = '\0';

  input = fopen(fileName, "r"); 

  if (input == NULL){
    printf("Fail to load %s\n", fileName);
    exit(0);
  }

  pos = ftell (input);
  fseek (input, 0, SEEK_END);
  end = ftell (input);
  fseek (input, pos, SEEK_SET);
  
  fclose(input);
  return temp+end;
}

//-------------------------------------------------------
int get_numLines(char* fileName){
  FILE* input;
  char buf[256];
  int numClusters = 0;
  int numLines = 0;
  int i;

  input = fopen(fileName, "r"); 

  if (input == NULL){
    printf("Fail to load %s\n", fileName);
    exit(0);
  }

  // get the number of clusters
  tokenizer(input, buf);
  numClusters = atoi(buf);

  numLines = 0;
  for (i = 0; i < numClusters; i++){
    tokenizer(input, buf);
    numLines += atoi(buf);
  }

  fclose(input);
  return numLines;
}

/* Copyright (c) 2008, 
 * Taekyung Kim, Jinha Kim, Minyoung Son, Hyeonseung Im, Sungwoo Park    
 * Programming Language Laboratory, POSTECH
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 * * Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer.
 * 
 * * Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution. 
 * 
 * * Neither the names of the original authors nor the names of any contributors
 * may be used to endorse or promote products derived from this software
 * without specific prior written permission.
 * 
 * * Modified source versions must be plainly marked as such, and must
 * not be misrepresented as being the original software.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
*/


