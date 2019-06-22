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
void read_data_files(const vector<string> v_fileNames);

void gen_vars(string var_name, vector<string> *v_vars);
void gen_vars2(string var_name, map<const string, int> *v_vars);
void print_struct_def();

string rename(string old);
string replace_all(string str, char c1, char c2);

void print_output_func();
void print_load_func();
void print_hashTable(int num_of_sequences);
void print_hash_func();

//---------------------------------------------------

int main(int argc, char** argv){
  int i;

  string config_fileName;

  vector<string> v_species;
  vector<string> v_fileNames;

  // output file open
  if (argc == 2){
    config_fileName = argv[1];
  }
  else{
    cout<<"Usage: dump config_fileName"<<endl;
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
  read_data_files(v_fileNames);

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
	// check whether the data file is 'a-b' or 'b-a'
        str_temp = FILE_DIRECTORY + header + v_species[i] + FILE_SEPARATOR + v_species[j];
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
void read_data_files(const vector<string> v_fileNames){
  string line;
  string token;
  int start_pos = 0;
  int i, j;
  int temp;
  string str_temp;
  int pre_cid, cid;
  int num_of_seqs;
  char ch;

  vector<int> v_num_of_seqs;
  
  // for each data files
  for (i = 0; i < v_fileNames.size(); i++){
    // cout<<"Opening "<<(v_fileNames[i])<<endl;
    cout << "."; 
    ifstream fd_data ((v_fileNames[i]).c_str());
    ofstream fd_data_out ((v_fileNames[i]+INTERMEDIATE_DATAFILE_SUFFIX).c_str());

    num_of_seqs = 0;

    if (fd_data.is_open()){
      pre_cid = -1;
      while (!fd_data.eof() ){
        getline (fd_data,line);

        start_pos = 0;
        while(start_pos < line.length()){
          // begin read data----------------
          // read CID
          str_temp = str_tokenizer(line, &start_pos);
          cid = atoi(str_temp.c_str());

          if (cid != pre_cid){ // new cluster
            if (pre_cid != -1)
              v_num_of_seqs.push_back(num_of_seqs);

            num_of_seqs = 1;
            pre_cid = cid;
          }
          else{
            num_of_seqs++;
          }


          // read score
          str_tokenizer(line, &start_pos);

          // read seq
          // species
          str_tokenizer(line, &start_pos);
          // seed
          str_tokenizer(line, &start_pos);
          // sequence name
          str_temp = str_tokenizer(line, &start_pos);

	  // ignore the rest of line
	  // XXX
	  start_pos = line.length();

          // end read data------------------
        }
      }
      if (num_of_seqs != 0)
        v_num_of_seqs.push_back(num_of_seqs);

      // print #clusters and #seqs---------
      fd_data_out<<v_num_of_seqs.size();
      for(j = 0; j < v_num_of_seqs.size(); j++){
        fd_data_out<<'\t'<<v_num_of_seqs[j];
      }
      fd_data_out<<endl;

      v_num_of_seqs.clear();

      // dump--------
      // cout<<"Dump"<<endl;
      fd_data.clear();
//    fd_data.seekg(0, ios::beg);
//    while(fd_data.get(ch)){
//      fd_data_out<<ch;
//    }
      fd_data.close();
      fd_data_out.close();
    }
    else{
      cout << "Unable to open file: "<<v_fileNames[i]<<endl;
      exit(-1);
    }
  }
  cout<<endl; 
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


