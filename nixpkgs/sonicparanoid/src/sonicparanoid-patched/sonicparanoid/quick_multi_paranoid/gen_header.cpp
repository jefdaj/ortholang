/* Copyright (c) 2008 
   Taekyung Kim, Jinha Kim, Minyoung Son, Hyeonseung Im, Sungwoo Park    
   Programming Language Laboratory, POSTECH
   gla@postech.ac.kr */

// myson@postech.ac.kr

#include <iostream>
#include <fstream>
#include <cstdlib>
#include <string>
#include <vector>
#include <algorithm>
#include <map>
#include <unistd.h>
#include "qp.h" 

using namespace std;

//---------------------------------------------------
void read_config_file(const string fileName, vector<string> *v_species);
string str_tokenizer(const string str, int *start_pos);
void gen_data_fileNames(const string header, const vector<string> v_species, 
                        vector<string> *v_fileNames);
void read_data_files(const vector<string> v_fileNames, map<const string, int> *map_seqNames,
                     int* max_length_of_sequence);

void gen_vars(string var_name, vector<string> *v_vars);
void gen_vars2(string var_name, map<const string, int> *v_vars, int* max_length_of_sequence);
void print_struct_def();

string renameHeader(string str_old);
string rename(string old);
string replace_all(string str, char c1, char c2);

void print_output_func();
void print_load_func();
void print_hashTable();
void print_hash_func();

//---------------------------------------------------

ofstream outFile_h;

int main(int argc, char** argv){
  int i;
  string config_fileName;

  vector<string> v_species;
  vector<string> v_fileNames;
  map<const string, int> map_seqNames;

  int max_length_of_sequence;

  // output file open
  if (argc == 3){
  // if (argc == 2){
    config_fileName = argv[1];
    outFile_h.open (argv[2]);
    // outFile_h.open (INTERMEDIATE_HEADER_FILE);
  }
  else{
    cout<<"Usage: gen_header.out config_fileName output_fileName"<<endl;
    // cout<<"Usage: gen_header config_fileName"<<endl;
    exit(0);
  }

  /*
   * parsing
   */
  // read a config file
  cout<<"Reading the config file"<<endl;
  read_config_file(config_fileName, &v_species);

  // generate data file names
  gen_data_fileNames(FILE_PREFIX, v_species, &v_fileNames);

  // read data files
  cout<<"Reading the data files"<<endl;
  read_data_files(v_fileNames, &map_seqNames, &max_length_of_sequence);

  /*
   * generating
   */
  // print structure definitions
  cout<<"Generating structure definitions"<<endl;
  print_struct_def();

  // print output function
  cout<<"Generating functions"<<endl;
  print_output_func();
  print_load_func();
  print_hash_func();
  print_hashTable();
  
  // generate species
  cout<<"Generating species"<<endl;
  gen_vars("species", &v_species);

  // generate sequences
  cout<<"Generating sequences"<<endl;
  gen_vars2("sequence", &map_seqNames, &max_length_of_sequence);

  outFile_h<<"#endif"<<endl;

  outFile_h.close();
  return 0;
}

//==============================================================================
void read_config_file(string fileName, vector<string> *v_species){
  string line;
  string token;
  int start_pos = 0;

  ifstream fd_config (fileName.c_str());

  if (fd_config.is_open()) {
    while (!fd_config.eof() )
      {
        getline (fd_config,line);

        start_pos = 0;
        while(start_pos < line.length())
          (*v_species).push_back(str_tokenizer(line, &start_pos));
      }
    fd_config.close();
  }
  else {
    cout << "Unable to open file: "<<fileName<<endl; 
    exit(-1);
  }
}

//==============================================================================
string str_tokenizer(const string str, int *start_pos){
  int pos = *start_pos;
  int start = pos;

  string str_temp;

  while (pos < str.length()){
    if (str[pos] == ' ' || str[pos] == '\t'){
      if (start < pos){
        str_temp = str.substr(start, pos-start);
        pos++;
        while (str[pos] == ' ' || str[pos] == '\t')
          pos++;
        *start_pos = pos;
        return str_temp;
      }
    }
    else{
      pos++;
    }
  }
  if (pos != start){
    *start_pos = pos;
    return str.substr(start, pos-start);
  }
}

//==============================================================================
void gen_data_fileNames(const string header, const vector<string> v_species, 
                        vector<string> *v_fileNames){
  int i, j;
  string str_temp;

  // generate data file names
  for (i = 0; i < v_species.size(); i++){
    for (j = 0; j < v_species.size(); j++){
      if ( i < j ){
        str_temp = FILE_DIRECTORY + header + v_species[i] + FILE_SEPARATOR +v_species[j];
	if( access(str_temp.c_str(),R_OK) != 0)
	  {
	    // use the alternative file name. The existence of the alternative file name is not checked. In case of that, read_data_files() will show an error message.
	    str_temp = FILE_DIRECTORY + header + v_species[j] + FILE_SEPARATOR + v_species[i];
	  }
        (*v_fileNames).push_back(str_temp);
      }
    }
  }
}

//==============================================================================
void read_data_files(const vector<string> v_fileNames, map<const string, int> *map_seqNames, int* max_length_of_sequence){
  string line;
  string token;
  int start_pos = 0;
  int i, j;
  int temp;
  string str_temp;
  int pre_cid, cid;
  int num_of_seqs;
  char ch;

  *max_length_of_sequence = 0;

  // for each data files
  for (i = 0; i < v_fileNames.size(); i++){
    cout<<"Opening "<<(v_fileNames[i])<<endl;
    ifstream fd_data ((v_fileNames[i]).c_str());

    pre_cid = -1;
    if (fd_data.is_open()){
      // skip the first line which contains the information of #clusters and #sequences (modified : not necessary)
      // getline (fd_data,line);

      while (!fd_data.eof() ){
        getline (fd_data,line);

        start_pos = 0;
        while(start_pos < line.length()){
          // begin read data----------------
          // read CID
          str_tokenizer(line, &start_pos);

          // read score
          str_tokenizer(line, &start_pos);

          // read seq
          // species
          str_tokenizer(line, &start_pos);
          // seed
          str_tokenizer(line, &start_pos);
          // sequence name
          str_temp = str_tokenizer(line, &start_pos);

          // insert new seqName to v_seqNames
          if ((*map_seqNames).count(str_temp) == 0){
            if ((temp = str_temp.length()) > *max_length_of_sequence)
              *max_length_of_sequence = temp;
            (*map_seqNames)[str_temp] = map_seqNames->size();
          }

	  // ignore the rest of line (e.g. bootstrap)
	  // XXX
	  start_pos = line.length();

          // end read data------------------
        }
      }
      fd_data.close();

    }
    else{
      cout << "Unable to open file: "<<v_fileNames[i]<<endl;
      exit(-1);
    }
  }
}

//==============================================================================
void gen_vars(string var_name, vector<string> *v_vars){
  int i;
  int num_vars = (*v_vars).size();

  // int num_vars = ...
  outFile_h<<"// num of "<<var_name<<endl;

  if (var_name.compare("species") == 0)
    outFile_h<<"#define\t NUM_OF_SPECIES\t"<<num_vars<<endl<<endl;
  else
    outFile_h<<"int num_of_"<<var_name<<"s = "<<num_vars<<";"<<endl<<endl;

  // vars id
  // int _0_aa = 0;
  // ...
  outFile_h<<"// "<<var_name<<" id"<<endl;
  outFile_h<<"// invariant: "<<var_name<<" id starts at 0."<<endl;
  for (i = 0; i < num_vars; i++){
    outFile_h<<"#define\t"<<rename((*v_vars)[i])<<"\t\t"<<i<<endl;
  }
  outFile_h<<endl;

  // int vars_table [] = {...};
  outFile_h<<"// "<<var_name<<" table"<<endl;
  outFile_h<<"static const int "<<var_name<<"_table [] = "<<endl;
  outFile_h<<"  {";
  for (i = 0; i < num_vars; i++){
    outFile_h<<rename((*v_vars)[i]);
    if (i == (num_vars - 1))
      outFile_h<<"};";
    else
      outFile_h<<", ";
    
    if ((i + 1) % 8 == 0)
      outFile_h<<"\n   ";
  }
  outFile_h<<endl<<endl;

  // int vars_names [] = {...};
  outFile_h<<"// "<<var_name<<" names"<<endl;
  outFile_h<<"// usage: "<<var_name<<"_names ["<<var_name<<" id]"<<endl;
  outFile_h<<"static const char* "<<var_name<<"_names [] = "<<endl;
  outFile_h<<"  {";
  for (i = 0; i < num_vars; i++){
    outFile_h<<"\""<<(*v_vars)[i]<<"\"";
    if (i == (num_vars - 1))
      outFile_h<<"};";
    else
      outFile_h<<", ";
    
    // for each 8 elements
    if ((i + 1) % 8 == 0)
      outFile_h<<"\n   ";
  }
  outFile_h<<endl<<endl;
}

//==============================================================================
void gen_vars2(string var_name, map<const string, int> *map_vars, int* max_length_of_sequence){
  int i;
  int num_vars = (*map_vars).size();
  map<string,int>::iterator iter;   

  // int num_vars = ...
  outFile_h<<"// num of "<<var_name<<endl;
  //outFile_h<<"int "<<var_name<<"_num = "<<num_vars<<";"<<endl<<endl;
  if (var_name.compare("species") == 0)
    outFile_h<<"int num_of_"<<var_name<<" = "<<num_vars<<";"<<endl<<endl;
  else
    outFile_h<<"#define\t NUM_OF_SEQUENCES\t"<<num_vars<<endl<<endl;

  // int vars_names [] = {...};
  outFile_h<<"// "<<var_name<<" names"<<endl;
  outFile_h<<"// usage: "<<var_name<<"_names ["<<var_name<<" id]"<<endl;
  outFile_h<<"char "<<var_name<<"_names[NUM_OF_SEQUENCES]["<<((*max_length_of_sequence) + 1)<<"]; "<<endl;

  outFile_h<<endl;
}

//==============================================================================
void print_struct_def(){
  outFile_h<<"#ifndef "<<renameHeader(INTERMEDIATE_HEADER_FILE)<<endl;
  outFile_h<<"#define "<<renameHeader(INTERMEDIATE_HEADER_FILE)<<endl;
  outFile_h<<"#include <stdio.h>"<<endl;
  outFile_h<<"#include <stdlib.h>"<<endl;
  outFile_h<<"#include <string.h>"<<endl;
  outFile_h<<"#include \"hashtable.h\""<<endl;
  outFile_h<<endl;
  outFile_h<<"#define NO 0 "<<endl;
  outFile_h<<"#define DIFF_NAMES 1 "<<endl;
  outFile_h<<"#define DIFF_NUMBERS 2 "<<endl;
  outFile_h<<endl;
  outFile_h<<"// sequence ---------------------------------------"<<endl;
  outFile_h<<"typedef struct{"<<endl;
  outFile_h<<"  int species_id;"<<endl;
  outFile_h<<"  double seed;"<<endl;
  outFile_h<<"  int sequence_id;"<<endl;
  outFile_h<<"} sequence;"<<endl;
  outFile_h<<""<<endl;
  outFile_h<<"// cluster ----------------------------------------"<<endl;
  outFile_h<<"typedef struct{"<<endl;
  outFile_h<<"  int score;"<<endl;
  outFile_h<<"  int num_of_sequences;"<<endl;
  outFile_h<<"  sequence *sequences;"<<endl;
  outFile_h<<"} cluster;"<<endl;
  outFile_h<<""<<endl;
  outFile_h<<"// dataFile ----------------------------------------"<<endl;
  outFile_h<<"// invariant: species_id1 must be prior to species_id2 in the species table."<<endl;
  outFile_h<<"typedef struct{"<<endl;
  outFile_h<<"  int species_id1;"<<endl;
  outFile_h<<"  int species_id2;"<<endl;
  outFile_h<<"  int num_of_clusters;"<<endl;
  outFile_h<<"  cluster *clusters;"<<endl;
  outFile_h<<"} dataFile;"<<endl;
  outFile_h<<endl;
  outFile_h<<"// cluster for output----------------------------------------"<<endl;
  outFile_h<<"typedef struct{"<<endl;
  outFile_h<<"  int tree_conflict;"<<endl;
  outFile_h<<"  int num_of_sequences;"<<endl;
  outFile_h<<"  sequence *sequences;"<<endl;
  outFile_h<<"} cluster_output;"<<endl;
  outFile_h<<""<<endl;
}

//==============================================================================
string renameHeader(string str){
  int i;

  // make uppercase and replace . with _
  for (i = 0; i < str.size(); i++){
    // if lowcase, make uppercase
    if (str[i] >= 97 && str[i] <=122)
      str[i] -=32;

    // replace . with _
    if (str[i] == '.')
      str[i] = '_';
  }

  return  str;
}

//==============================================================================
string rename(string str_old){
  string str_temp;
  string rename_header = "_";

  str_temp = (rename_header + replace_all(str_old, '.', '_'));
  return  str_temp;
}

//==============================================================================
string replace_all(string str, char c1, char c2){
  int i;

  for (i = 0; i < str.size(); i++){
    if (str[i] == c2)
      str[i] = 'u';
    if (str[i] == c1 || str[i] == '-')
      str[i] = c2;

  }
  return str;
}

//==============================================================================
void print_output_func(){
  // int compare
  // void output_result
  outFile_h<<"//-------------------------------------------------------"<<endl;
  outFile_h<<"// output_result takes a cluster_output array and its size"<<endl;
  outFile_h<<"//           and prints the elements of each cluster_ouput."<<endl;
  outFile_h<<"void output_result(int size, cluster_output *clusters);"<<endl;
  outFile_h<<endl;                                                                         
}

//==============================================================================
void print_load_func(){
  outFile_h<<"//-------------------------------------------------------"<<endl;
  outFile_h<<"// load_dataFile takes a pair of species ids and a dataFile"<<endl;
  outFile_h<<"//           and parses and loads the data file of two species."<<endl;
  outFile_h<<"// invariant: species_id1 < species_id2"<<endl;
  outFile_h<<"void load_dataFile(int species_id1, int species_id2, dataFile* data);"<<endl;
  outFile_h<<endl;                                                                         
  outFile_h<<"// free_dataFile takes a pointer of a datafile and frees all memory."<<endl;
  outFile_h<<"void free_dataFile(dataFile* data);"<<endl;
  outFile_h<<endl;
  outFile_h<<endl;                                                                         
}

//==============================================================================
void print_hashTable(){
  outFile_h<<"// hash typedef"<<endl;
  outFile_h<<"typedef char h_key_t;"<<endl;
  outFile_h<<"typedef int h_value_t;"<<endl;
  outFile_h<<endl;
  outFile_h<<"static int is_init_hash_tables = 0;"<<endl;
  outFile_h<<"struct hashtable* ht_speciesName2Id;"<<endl;
  outFile_h<<"struct hashtable* ht_seqName2Id;"<<endl;
  outFile_h<<endl;
}

//==============================================================================
void print_hash_func(){
  outFile_h<<"// free hash tables"<<endl;
  outFile_h<<"void free_hashtables();"<<endl;
  outFile_h<<endl;
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


