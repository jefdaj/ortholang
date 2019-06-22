/* Copyright (c) 2008
   Taekyung Kim, Jinha Kim, Minyoung Son, Hyeonseung Im, Sungwoo Park
   Programming Language Laboratory, POSTECH
   gla@postech.ac.kr */

// myson@postech.ac.kr

#include <iostream>
#include <iomanip>
#include <fstream>
#include <cstdlib>
#include <string>
#include <vector>
#include <algorithm>
#include "qa.h"
#include "string.h"

using namespace std;

#define NO 0
#define DIFF_NAMES 1
#define DIFF_NUMBERS 2

#define NEQ 0
#define EQ_ONLY_GENE 1
#define EQ_GENE_CONFLICT 2

#define MAX_NUM_SPECIES 1000

// sequence ----------------------------------------
typedef struct{
  string species;
  string gene;
  bool is_seed_ortholog;
  double confidence_score;
} struct_sequence;

// cluster ----------------------------------------
typedef struct{
  int cluster_id;
  string species_in_cluster;
  int tree_conflict;
  vector<struct_sequence> v_sequences;
} struct_cluster;

// conflict struct -------------------------------
typedef struct{
  int cluster_id1;
  int cluster_conflict1;
  int cluster_id2;
  int cluster_conflict2;
} struct_conflict;

// ----------------------------------------
string str_tokenizer(const string str, int *start_pos);
void read_output_file(string fileName, vector<struct_cluster> *v_clusters);
void print_v_clusters(vector<struct_cluster>* v_clusters);
string getStringConflict(int treeConflict);
int find_num_species(string str, string separator);
int get_info(vector<struct_cluster>* v_clusters, int* numClusterPerSpecies, string separator);

int main(int argc, char** argv){
  vector<struct_cluster> v_clusters;
  string out_fileName;
  int numClusterPerSpecies[MAX_NUM_SPECIES] = {0};
  int numSpecies;
  int i;
  int total = 0;

  // output file open
  if (argc == 2){
    out_fileName = argv[1];
  }
  else{
    cout<<"Usage: "<<argv[0]<< " <output file>"<<endl;
    cout<<argv[0]<<" analyzes <output file> produced by ortholog clustering. "
     <<endl;
    exit(0);
  }

  // parsing
  read_output_file(out_fileName, &v_clusters);

  // get the number of cluster whose species's number is the same
  numSpecies = get_info(&v_clusters, numClusterPerSpecies, SPECIES_SEPARATOR);

  for (i = 1; i <= numSpecies; i++){
    total += numClusterPerSpecies[i];
    cout << "Number of clusters consisting of " <<i<<" species : "<<numClusterPerSpecies[i]<<endl;
  }
  cout<<"Total: "<<total<<endl;

  return 0;
}

//==============================================================================
int find_num_species(string str, string separator){
  int i;
  int count = 0;
  int found_idx = 0;

  while((found_idx = str.find(separator, found_idx)) != string::npos){
    found_idx++;
    count++;
  }

  return count;
}

//==============================================================================
int get_info(vector<struct_cluster>* v_clusters, int* numClusterPerSpecies, string separator){
  int i;
  int numOfSpecies;
  int numMaxSpecies = 0;;

  // for each cluster
  for (i = 0; i < v_clusters->size(); i++){
    numOfSpecies = find_num_species((*v_clusters)[i].species_in_cluster, separator) + 1;

    if (numOfSpecies > numMaxSpecies)
      numMaxSpecies = numOfSpecies;

    numClusterPerSpecies[numOfSpecies]++;
  }

  return numMaxSpecies;
}

//==============================================================================
// modify this function to change parsing
void read_output_file(string fileName, vector<struct_cluster> *v_clusters){
  string line;
  string token;
  int start_pos = 0;
  int cluster_id;
  string str_temp, str_temp2;

  struct_cluster temp_cluster;
  temp_cluster.cluster_id = -1;

  ifstream fd_output (fileName.c_str());

  if (fd_output.is_open()) {
    while (!fd_output.eof() )
      {
        struct_sequence temp_sequence;

        getline (fd_output, line);

        if (line[0] == '#'){
          continue;
        }

        start_pos = 0;
        // parsing for each line
        while(start_pos < line.length()){

          // get cluster id
          str_temp = str_tokenizer(line, &start_pos);
          if (temp_cluster.cluster_id != (cluster_id = atoi(str_temp.c_str()))){
            if (temp_cluster.cluster_id != -1)
              (*v_clusters).push_back(temp_cluster);
            temp_cluster.v_sequences.clear();
            temp_cluster.cluster_id = cluster_id;
          }

          // get species
          temp_sequence.species = str_tokenizer(line, &start_pos);

          // get gene
          temp_sequence.gene = str_tokenizer(line, &start_pos);

          // get is_seed_ortholog
          str_temp = str_tokenizer(line, &start_pos);
          if (atoi(str_temp.c_str()) == 1)
            temp_sequence.is_seed_ortholog = true;
          else
            temp_sequence.is_seed_ortholog = false;

          // get confindecne_score
          temp_sequence.confidence_score = atof((str_tokenizer(line, &start_pos)).c_str());

          if (temp_cluster.v_sequences.size() == 0){ // only in the beginning of cluster
            // get species_in_cluster
            temp_cluster.species_in_cluster = str_tokenizer(line, &start_pos);

            // get tree_conflict
            str_temp = str_tokenizer(line, &start_pos);
            if (str_temp.compare("No")==0 || str_temp.compare("NO")==0)
              temp_cluster.tree_conflict = NO;
            else if (str_temp.compare("diff.")==0){
              str_temp2 = str_tokenizer(line, &start_pos);
              if (str_temp2.compare("numbers")==0)
                temp_cluster.tree_conflict = DIFF_NUMBERS;
              else if (str_temp2.compare("names")==0)
                temp_cluster.tree_conflict = DIFF_NAMES;
              else{
                cout<<"parsing error1"<<endl;
                exit(-1);
              }
            } else{
              cout<<"parsing error2"<<endl;
              exit(-1);
            }
          }
          else{ // ignore species_in_cluster and tree conflict
            // get species_in_cluster
            str_tokenizer(line, &start_pos);

            // get tree_conflict
            str_temp = str_tokenizer(line, &start_pos);
            if (str_temp.compare("diff.")==0)
              str_temp2 = str_tokenizer(line, &start_pos);
          }
        }
        if (line.length() != 0)
          temp_cluster.v_sequences.push_back(temp_sequence);
      }
    (*v_clusters).push_back(temp_cluster);
    fd_output.close();
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
string getStringConflict(int treeConflict){
  if (treeConflict == NO)
    return "NO";
  else if (treeConflict == DIFF_NAMES)
    return "diff. names";
  else if (treeConflict == DIFF_NUMBERS)
    return "diff. numbers";
  else
    return "not reached";
}

//==============================================================================
void print_v_clusters(vector<struct_cluster> *v_clusters){
  int i, j;

  for (i = 0; i < v_clusters->size(); i++){
    // print each cluster
    cout<<(*v_clusters)[i].cluster_id<<endl;
  }
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
