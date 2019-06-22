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
extern "C" {
#include "hashtable.h"
}
#include "string.h"

using namespace std;

#define NO 0
#define DIFF_NAMES 1
#define DIFF_NUMBERS 2

#define NEQ 0
#define EQ_ONLY_GENE 1
#define EQ_GENE_CONFLICT 2

typedef char h_key_t;
typedef int h_value_t;

// sequence ----------------------------------------
typedef struct{
  string species;
  int gene;
  bool is_seed_ortholog;
  double confidence_score;
} struct_sequence;

// cluster ----------------------------------------
typedef struct{
  int cluster_id;
  string species_in_cluster;
  int tree_conflict;
  int size_v_seqs;
  vector<struct_sequence> v_sequences;
} struct_cluster;

// conflict struct -------------------------------
typedef struct{
  int cluster_id1;
  int cluster_conflict1;
  int cluster_id2;
  int cluster_conflict2;
} struct_conflict;

// conflict struct -------------------------------
typedef struct{
  vector<struct_cluster> v_residue_clusters1;
  vector<struct_cluster> v_residue_clusters2;
  vector<struct_conflict> v_conflict_clusters;
  int numConflictTable[3][3];
} struct_result;

// ----------------------------------------
string str_tokenizer(const string str, int *start_pos);
void read_output_file(string fileName, vector<struct_cluster> *v_clusters, struct hashtable* ht_seqName2Id);

bool cmp_sequences(struct_sequence seq1, struct_sequence seq2);
void sort_sequences(vector<struct_sequence>* v_sequences);

bool cmp_clusters(struct_cluster c1, struct_cluster c2);
void sort_v_clusters(vector<struct_cluster>* v_clusters);

bool compare_sequences(struct_sequence* sequence1, struct_sequence* sequence2, int option);
int compare_clusters(struct_cluster* cluster1, struct_cluster* cluster2);
struct_result* compare_v_clusters(vector<struct_cluster>* v_clusters1, vector<struct_cluster>* v_clusters2, int option);

void print_cmp_result(struct_result* cmp_result, int size_v_clusters1, int size_v_clusters2, int option);

void print_v_clusters(vector<struct_cluster>* v_clusters);
void print_v_conflict(vector<struct_conflict> *v_conflict);
string getStringConflict(int treeConflict);

// hash functions
unsigned int hashfromkey(void *k);
int equalkeys(void* k1, void* k2);

string out_fileName1, out_fileName2;

int main(int argc, char** argv){
  vector<struct_cluster> v_clusters1, v_clusters2;
  struct_result* cmp_result = NULL;
  int option;
  int i;
  int size_v_clusters1;
  int size_v_clusters2;

  struct hashtable* ht_seqName2Id;

  // output file open
  if (argc == 3){
    out_fileName1 = argv[1];
    out_fileName2 = argv[2];
    option = EQ_ONLY_GENE;
  } else  if (argc == 4){
    out_fileName1 = argv[1];
    out_fileName2 = argv[2];
    option = atoi (argv[3]);
    if (option != EQ_ONLY_GENE && option != EQ_GENE_CONFLICT)
      option = EQ_ONLY_GENE;
  }
  else{
    cout<<"Usage: "<<argv[0]<< " <output file 1> <output file 2> [option]"<<endl;
    cout<<argv[0]<<" compares <output file 1> and <output file 2> produced by ortholog clustering."<<endl;
    cout<<"Option: 1 - consider only sequence names to compare clusters (default)"<<endl;
    cout<<"        2 - consider both sequence names and tree conflicts to compare clusters"<<endl;
    exit(0);
  }

  ht_seqName2Id = create_hashtable(100000, hashfromkey, equalkeys);

  // parsing
  read_output_file(out_fileName1, &v_clusters1, ht_seqName2Id);
  read_output_file(out_fileName2, &v_clusters2, ht_seqName2Id);

  // sorting
  for(i = 0; i < v_clusters1.size(); i++){
    sort_sequences(&(v_clusters1[i].v_sequences));
  }
  sort_v_clusters(&v_clusters1);

  for(i = 0; i < v_clusters2.size(); i++){
    sort_sequences(&(v_clusters2[i].v_sequences));
  }
  sort_v_clusters(&v_clusters2);

  // get sizes
  size_v_clusters1 = v_clusters1.size();
  size_v_clusters2 = v_clusters2.size();

  // comparing
  if (option == EQ_ONLY_GENE)
    cout<<"Checking only sequence names..."<<endl;
  else if (option == EQ_GENE_CONFLICT)
    cout<<"Checking both sequence names and tree conflicts..."<<endl;

  cmp_result = compare_v_clusters(&v_clusters1, &v_clusters2, option);

  // print cmp_result
  print_cmp_result(cmp_result, size_v_clusters1, size_v_clusters2, option);

  delete cmp_result;
  return 0;
}

//==============================================================================
bool cmp_sequences(struct_sequence seq1, struct_sequence seq2){
  return (seq1.gene < seq2.gene);
}

//==============================================================================
void sort_sequences(vector<struct_sequence>* v_sequences){
  sort(v_sequences->begin(), v_sequences->end(), cmp_sequences);
}

//==============================================================================
bool cmp_clusters(struct_cluster c1, struct_cluster c2){
  if ((c1.size_v_seqs) < (c2.size_v_seqs))
    return true;
  if ((c1.size_v_seqs) == (c2.size_v_seqs)){
    if (cmp_sequences(c1.v_sequences[0], c2.v_sequences[0]))
      return true;
    else
      return false;
  } else
    return false;
}

//==============================================================================
void sort_v_clusters(vector<struct_cluster>* v_clusters){
  sort(v_clusters->begin(), v_clusters->end(), cmp_clusters);
}

//==============================================================================
bool compare_sequences(struct_sequence* sequence1, struct_sequence* sequence2){
  if (sequence1->gene == sequence2->gene
      //sequence1->species == sequence2->species &&
      //sequence1->is_seed_ortholog == sequence2->is_seed_ortholog &&
      //sequence1->confidence_score == sequence2->confidence_score &&
      )
    return true;
  else
    return false;
}

//==============================================================================
int compare_clusters(struct_cluster* cluster1, struct_cluster* cluster2){
  int i = 0;

  for (i = 0; i < cluster1->size_v_seqs; i++){
    if (!(compare_sequences(&(cluster1->v_sequences[i]), &(cluster2->v_sequences[i]))))
      return NEQ;
  }

  if (cluster1->tree_conflict == cluster2->tree_conflict)
    return EQ_GENE_CONFLICT;
  else
    return EQ_ONLY_GENE;
}

//==============================================================================
struct_result* compare_v_clusters(vector<struct_cluster>* v_clusters1, vector<struct_cluster>* v_clusters2, int option){
  struct_result* cmp_result = NULL;
  int i, j;

  struct_conflict temp_struct_conflict ;
  int compare_result;

  cmp_result = new struct_result;

  // initialize numConflictTable
  for (i = 0; i < 3; i++){
    for (j = 0; j < 3; j++){
      cmp_result->numConflictTable[i][j] = 0;
    }
  }

  for (i = 0; i < v_clusters1->size(); i++){

    if (v_clusters2->size() == 0){
      cmp_result->v_residue_clusters1.push_back((*v_clusters1)[i]);
      continue;
    }

    for (j = 0; j < v_clusters2->size(); j++){
      if ((*v_clusters1)[i].size_v_seqs < (*v_clusters2)[j].size_v_seqs){
        cmp_result-> v_residue_clusters1.push_back((*v_clusters1)[i]);
        break;
      } else if ((*v_clusters1)[i].size_v_seqs > (*v_clusters2)[j].size_v_seqs){
        cmp_result-> v_residue_clusters2.push_back((*v_clusters2)[j]);
        v_clusters2->erase((v_clusters2->begin())+j);
        j--;
        if (v_clusters2->size() == 0)
          cmp_result->v_residue_clusters1.push_back((*v_clusters1)[i]);
        continue;
      } else{
        if (((*v_clusters1)[i].v_sequences[0].gene) > ((*v_clusters2)[j].v_sequences[0].gene)){
          cmp_result-> v_residue_clusters2.push_back((*v_clusters2)[j]);
          v_clusters2->erase((v_clusters2->begin())+j);
          j--;
          if (v_clusters2->size() == 0)
            cmp_result->v_residue_clusters1.push_back((*v_clusters1)[i]);
          continue;
        } else if (((*v_clusters1)[i].v_sequences[0].gene) < ((*v_clusters2)[j].v_sequences[0].gene)){
          cmp_result-> v_residue_clusters1.push_back((*v_clusters1)[i]);
          break;
        } else{
          compare_result = compare_clusters(&((*v_clusters1)[i]), &((*v_clusters2)[j]));

          // invariant: There is no cluster whose genes are the same in a cluster list.
          if (compare_result == EQ_ONLY_GENE || compare_result == EQ_GENE_CONFLICT){
            //cout<<"insert i "<<i<<" and j"<<j<<" are the same, then erase j"<<endl;
            // check conflict cases if option is set
            if (option == EQ_GENE_CONFLICT && compare_result == EQ_ONLY_GENE){
              temp_struct_conflict.cluster_id1 = (*v_clusters1)[i].cluster_id;
              temp_struct_conflict.cluster_conflict1 = (*v_clusters1)[i].tree_conflict;
              temp_struct_conflict.cluster_id2 = (*v_clusters2)[j].cluster_id;
              temp_struct_conflict.cluster_conflict2 = (*v_clusters2)[j].tree_conflict;

              cmp_result->v_conflict_clusters.push_back(temp_struct_conflict);
            }

            // update numConflict
            cmp_result->numConflictTable[(*v_clusters1)[i].tree_conflict][(*v_clusters2)[j].tree_conflict]++;

            v_clusters2->erase((v_clusters2->begin())+j);
            break;
          }
        }
      }
    }

  }

  for(i = 0; i < v_clusters2->size(); i++){
    cmp_result-> v_residue_clusters2.push_back((*v_clusters2)[i]);
  }

  return cmp_result;
}

//==============================================================================
void print_cmp_result(struct_result* cmp_result, int size_v_clusters1, int size_v_clusters2, int option){
  int i, j;
  int num_matched_clusters =  size_v_clusters1 - (cmp_result->v_residue_clusters1.size());

  cout<<"Number of clusters in "<< out_fileName1 <<" : "<< size_v_clusters1 <<endl;
  cout<<"Number of clusters in "<< out_fileName2 <<" : "<< size_v_clusters2 <<endl;
  cout<<"Number of matched clusters: "<<num_matched_clusters<<endl;
  if (option == EQ_GENE_CONFLICT){
    cout<<"Number of matched clusters with the same tree conflict: "
        << num_matched_clusters - (cmp_result->v_conflict_clusters.size()) <<endl;
    cout<<"Number of matched clusters with different tree conflicts: "
        << cmp_result->v_conflict_clusters.size() <<endl;
  }

  if (option == EQ_GENE_CONFLICT){
    cout<<"Summary of matched clusters:"<<endl;
    for(i = 0; i < 3; i++){
      for(j = 0; j < 3; j++){
        cout<<left<<setw(15)<<getStringConflict(i)<<" "<<left<<setw(15)<<getStringConflict(j)
            <<cmp_result->numConflictTable[i][j]<<endl;
      }
    }
  }

  cout<<"Residue clusters in "<< out_fileName1 << ":"<< endl;
  if (cmp_result->v_residue_clusters1.size() == 0)
    cout<<"None"<<endl;
  else
    print_v_clusters(&(cmp_result->v_residue_clusters1));

  cout<<"Residue clusters in "<< out_fileName2 << ":" << endl;
  if (cmp_result->v_residue_clusters2.size() == 0)
    cout<<"None"<<endl;
  else
    print_v_clusters(&(cmp_result->v_residue_clusters2));

  if (option == EQ_GENE_CONFLICT){
    cout<<"Matched clusters with different tree conflicts:"<<endl;
    if (cmp_result->v_conflict_clusters.size() == 0)
      cout<<"None"<<endl;
    else
      print_v_conflict(&(cmp_result->v_conflict_clusters));
  }
}

//==============================================================================
string getStringConflict(int treeConflict){
  if (treeConflict == NO)
    return "No";
  else if (treeConflict == DIFF_NAMES)
    return "Diff. name";
  else if (treeConflict == DIFF_NUMBERS)
    return "Diff. number";
  else
    return "Unknown";
}

//==============================================================================
void print_v_conflict(vector<struct_conflict> *v_conflict){
  int i;

  for(i = 0; i < v_conflict->size(); i++){
    cout<<(*v_conflict)[i].cluster_id1<<" ("<<getStringConflict((*v_conflict)[i].cluster_conflict1)<< ") - "
        << (*v_conflict)[i].cluster_id2<<" ("<<getStringConflict((*v_conflict)[i].cluster_conflict2)<<")"<<endl;
  }
}

//==============================================================================
void print_v_clusters(vector<struct_cluster> *v_clusters){
  int i, j;

  for (i = 0; i < v_clusters->size(); i++){
    // print each cluster
    cout<<(*v_clusters)[i].cluster_id<< " ";
  }
  cout << endl;
}

//==============================================================================
// modify this function to change parsing
void read_output_file(string fileName, vector<struct_cluster> *v_clusters, struct hashtable* ht_seqName2Id){
  string line;
  string token;
  int start_pos = 0;
  int cluster_id;
  string str_temp, str_temp2;

  h_key_t* k;
  h_value_t* v;
  h_value_t* found_v;
  int cur_sequence_id = hashtable_count(ht_seqName2Id);
  char buf[120];

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
            if (temp_cluster.cluster_id != -1){
              temp_cluster.size_v_seqs = temp_cluster.v_sequences.size();
              (*v_clusters).push_back(temp_cluster);
            }
            temp_cluster.v_sequences.clear();
            temp_cluster.cluster_id = cluster_id;
          }

          // get species
          temp_sequence.species = str_tokenizer(line, &start_pos);

          // get gene
          //temp_sequence.gene = str_tokenizer(line, &start_pos);
          str_temp = str_tokenizer(line, &start_pos);
          strcpy(buf, str_temp.c_str());

          if ((found_v = (int*) hashtable_search(ht_seqName2Id, buf)) == NULL){
            // make a key and a value
            k = (h_key_t*) malloc ((str_temp.size() +1)* sizeof(h_key_t));
            strcpy(k, buf);
            v = (h_value_t*) malloc (sizeof(h_value_t));
            *v  = cur_sequence_id;
            // add them to hash table
            hashtable_insert(ht_seqName2Id, k, v);
            //temp_sequence_id = cur_sequence_id;
            temp_sequence.gene = cur_sequence_id;

            cur_sequence_id++;
          } else{
            temp_sequence.gene = *found_v;
          }

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
                cerr<<"Parsing error1"<<endl;
                exit(-1);
              }
            } else{
              cerr<<"Parsing error2: You should annotate '#' on comments."<<endl;
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
    temp_cluster.size_v_seqs = temp_cluster.v_sequences.size();
    (*v_clusters).push_back(temp_cluster);
    fd_output.close();
  }
  else {
    cerr << "Unable to open file: "<<fileName<<endl;
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

//-------------------------------------------------------
// hash functions
unsigned int hashfromkey(void *k){
  unsigned int hash = 5381;
  int c;

  h_key_t* kk = (h_key_t*) k;

  while (c = *kk++)
    hash = ((hash << 5) + hash) + c; /* hash * 33 + c */

  return hash;
}

//-------------------------------------------------------
int equalkeys(void* k1, void* k2){
  h_key_t* kk1 = (h_key_t*) k1;
  h_key_t* kk2 = (h_key_t*) k2;

  return (0 == strcmp(kk1, kk2));
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
