/* Copyright (c) 2008 
   Taekyung Kim, Jinha Kim, Minyoung Son, Hyeonseung Im, Sungwoo Park    
   Programming Language Laboratory, POSTECH
   gla@postech.ac.kr */

#include <unistd.h>
#include "qp.h" 
#include "qa.h"
#include INTERMEDIATE_HEADER_FILE         


static int cur_sequence_id = 0;

//-------------------------------------------------------
void free_hashtables(){
  hashtable_destroy(ht_speciesName2Id, 1);
  hashtable_destroy(ht_seqName2Id, 1);
}

//-------------------------------------------------------
// an auxiliary fuction for output_result
int compare (const void * a, const void * b){
  return ( *(int*)a - *(int*)b );
}

//-------------------------------------------------------
// an auxiliary fuction for output_result
void get_species_set(cluster_output cluster, char *set_name){
  int i, j;
  int temp;

  int num_of_sids = 0;
  int *species_ids;

  species_ids = (int *) malloc ((cluster.num_of_sequences) * sizeof(int));

  // get species id
  for(i = 0; i < cluster.num_of_sequences; i++){
    temp = cluster.sequences[i].species_id;
    // check existing species id
    for(j = 0; j < num_of_sids; j++){
      if (species_ids[j] == temp)
        break;
    }
    // if new species id
    if (j == num_of_sids){
      species_ids[num_of_sids] = temp;
      num_of_sids++;
    }
  }

  // sort
  qsort(species_ids, num_of_sids, sizeof(int), compare);

  // generate species set
  strcpy(set_name,"");
  for (i = 0; i< num_of_sids; i++){
    strcat(set_name, species_names[species_ids[i]]);
    if (i != num_of_sids - 1)
      strcat(set_name, SPECIES_SEPARATOR);
  }
  free (species_ids);
}

//-------------------------------------------------------
// output_result takes a cluster_output array and its size
//           and prints the elements of each cluster_ouput.
void output_result(int size, cluster_output *clusters){
  int i, j;
  char *set_name;
  int size_of_set_name = 0;

  // set size of set_name
  for (i = 0; i< NUM_OF_SPECIES; i++){
    size_of_set_name += strlen (species_names [i]) + 1;
  }
  set_name = (char*) malloc(size_of_set_name * sizeof(char));

  printf("#");
  printf("clusterID\t");
  printf("species\t");
  printf("gene\t");
  printf("is_seed_ortholog\t");
  printf("confidence_score\t");
  printf("species_in_cluster\t");
  printf("tree_conflict\t\n");

  // print clusters
  for(i = 0; i < size; i++){
    get_species_set(clusters[i], set_name);
    // print sequences
    for(j = 0; j < clusters[i].num_of_sequences; j++){
      printf("%d\t", i+1);
      printf("%s\t", species_names[clusters[i].sequences[j].species_id]);
      printf("%s\t", sequence_names[clusters[i].sequences[j].sequence_id]);
      printf("%d\t", ((int)(clusters[i].sequences[j].seed) ? 1 : 0));
      printf("%.3lf\t", clusters[i].sequences[j].seed);
      printf("%s\t", set_name);
      if (clusters[i].tree_conflict == NO)
        printf("No\n");
      else if (clusters[i].tree_conflict == DIFF_NAMES)
        printf("diff. names\n");
      else if (clusters[i].tree_conflict == DIFF_NUMBERS)
        printf("diff. numbers\n");
      else
        printf("Error\n");
    }
  }

  free (set_name);
}

//-------------------------------------------------------
void get_dataFileName(char* header, int species_id1, int species_id2, char* name){
  name = strcpy(name, FILE_DIRECTORY);
  name = strcat(name, header);
  name = strcat(name, species_names[species_id1]);
  name = strcat(name, FILE_SEPARATOR);
  name = strcat(name, species_names[species_id2]);
  name[strlen(name)] = '\0';
}

//-------------------------------------------------------
#define LINE_INIT_SIZE 256
char* read_line(FILE *input, char *buf, int* buf_len)
{
  int ch;
  int pos = 0;

  if( buf == NULL )
    {
      buf = (char*)malloc(sizeof(char)*LINE_INIT_SIZE);
      if ( buf == NULL )
	{
	  return buf;
	}
      *buf_len = LINE_INIT_SIZE;
    }

  ch = getc( input );
  
  // trim white space
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

  // read_line
  while(ch != '\n')
    {
      if( pos >= *buf_len )
	{
	  buf = (char*)realloc(buf,sizeof(char)*(*buf_len+LINE_INIT_SIZE));
	  if( buf == NULL )
	    {
	      return buf;
	    }
	  *buf_len += LINE_INIT_SIZE;
	}

      if( ch == EOF )
	{
	  break;
	}
      buf[pos] = ch;
      pos++;
      ch = getc(input);
    }

  buf[pos] = '\0';


  return buf;


}

//-------------------------------------------------------
int line_tokenizer(char *line_buf, int *pos, char* buf){
  char ch;
  int buf_p = 0;
  int line_len = strlen(line_buf);

  if( *pos >= line_len)
    {
      return 0;
    }

  // trim white space
  while(line_buf[*pos] == ' ' || line_buf[*pos] == '\t') {
    (*pos)++;
    if( *pos >= line_len )
      {
	return 0;
      }
  }       


  // tokenize
  while(line_buf[*pos] != ' ' && line_buf[*pos] != '\t' && *pos < line_len) {
    //printf("%c", ch );
    buf[buf_p] = line_buf[*pos];
    buf_p++;
    (*pos)++;
  }       

  // make the last char be null
  buf[buf_p]='\0';


  return buf_p;
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
void print_dataFile(dataFile* data){
  int i, j;

  for (i = 0; i < data->num_of_clusters; i++){
    for (j = 0; j < (data->clusters[i]).num_of_sequences; j++){
      printf("%d\t", i+1);
      printf("%d\t", (data->clusters[i]).score);
      printf("%s\t", species_names[(data->clusters[i]).sequences[j].species_id]);
      printf("%.3lf\t", (data->clusters[i]).sequences[j].seed);
      printf("%s\n", sequence_names[(data->clusters[i]).sequences[j].sequence_id]);
    }
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

//-------------------------------------------------------
void free_dataFile(dataFile* data){
  int i;

  for(i = 0; i < data->num_of_clusters; i++){
    free((data->clusters[i]).sequences);
  }
  free(data->clusters);
  free (data);
}

//-------------------------------------------------------
void init_hash_tables(){
  int i;
  char* k;
  int* v;

  ht_speciesName2Id = create_hashtable(NUM_OF_SPECIES, hashfromkey, equalkeys);
  ht_seqName2Id = create_hashtable(NUM_OF_SEQUENCES, hashfromkey, equalkeys);

  if (ht_speciesName2Id == NULL || ht_seqName2Id == NULL){
    printf("out of memery: hash table\n");
    exit(-1);
  }

  // make a hashtable for species names
  for(i = 0; i < NUM_OF_SPECIES; i++){
    k = (h_key_t*) malloc ((strlen(species_names[i]) + 1)* sizeof(h_key_t));
    strcpy(k, species_names[i]);
    v = (h_value_t*) malloc (sizeof(h_value_t));
    *v  = i;

    hashtable_insert(ht_speciesName2Id,k,v);
  }
}

//-------------------------------------------------------
void load_dataFile(int species_id1, int species_id2, dataFile* data){
  int i, j;  
  char* fileName;
  char* dumpFileName;
  char buf[256];
  char* line_buf = NULL;
  int line_len = -1;
  int line_pos = 0;
  FILE* input;

  h_key_t* k;
  h_value_t* v;
  h_value_t* found_v;
  int temp_sequence_id;

  // make a hashtable for mapping a species name to its id
  if (is_init_hash_tables == 0){
    init_hash_tables();
    is_init_hash_tables = 1;
  }

  // invariant: species_id1 < species_id2
  if (species_id1 == species_id2){
    printf("invariant: species_id1 < species_id2\n");
    exit(0);
  } else if (species_id1 > species_id2){
    i = species_id2;
    species_id2 = species_id1;
    species_id1 = i;
  }

  if (species_id1 > (NUM_OF_SPECIES - 1) || species_id2 > (NUM_OF_SPECIES - 1)){
    printf("Invalid species id");
    exit(0);
  }

  // get a data file name
  fileName = (char*) malloc ((strlen(FILE_DIRECTORY) + strlen(FILE_PREFIX) + strlen(species_names[species_id1]) 
                              + strlen(species_names[species_id2]) + 2) * sizeof(char));
  get_dataFileName(FILE_PREFIX, species_id1, species_id2, fileName);

  if( access(fileName,R_OK) != 0)
    {
      get_dataFileName(FILE_PREFIX, species_id2, species_id1, fileName);
    }


 
  // get a dump file name
  dumpFileName = (char*) malloc ((strlen(fileName) + strlen(INTERMEDIATE_DATAFILE_SUFFIX) + 2) * sizeof(char));  
  dumpFileName = strcpy(dumpFileName,fileName);
  dumpFileName = strcat(dumpFileName,INTERMEDIATE_DATAFILE_SUFFIX);
  dumpFileName[strlen(dumpFileName)] = '\0';

  // open the dump file
  input = fopen(dumpFileName, "r"); 
  if (input == NULL){
    printf("fail to load %s\n", dumpFileName);
    exit(0);
  }

  // init ------------------------
  data->species_id1 = species_id1;
  data->species_id2 = species_id2;
  // parsing the first line which contains #clusters and #sequences
  // #cluster
  tokenizer(input, buf);
  data->num_of_clusters = atoi(buf);
  data->clusters = (cluster*) malloc ((data->num_of_clusters) * sizeof(cluster));
  for (i = 0; i < data->num_of_clusters; i++){
    // #sequences
    tokenizer(input, buf);
    (data->clusters[i]).num_of_sequences = atoi(buf);
    (data->clusters[i]).sequences = 
      (sequence*) malloc (((data->clusters[i]).num_of_sequences) * sizeof(sequence));
  }
  free(dumpFileName);
  fclose(input);
  
  // open the data file
  input = fopen(fileName, "r");
  if (input == NULL){
	  printf("fail to load %s\n", fileName);
	  exit(0);
  }				

  for (i = 0; i < data->num_of_clusters; i++){
    for (j = 0; j < (data->clusters[i]).num_of_sequences; j++){
      line_pos = 0;
      line_buf = read_line(input,line_buf,&line_len);
      if( line_buf == NULL )
	{
	  printf("fail to read a line\n");
	  exit(0);
	}

      // get an cluster id-----------
      line_tokenizer(line_buf, &line_pos, buf);

      // get an inparanoid score-----------
      line_tokenizer(line_buf, &line_pos, buf);
      if (j == 0)
        (data->clusters[i]).score = atoi(buf);

      // get a species------------------------
      line_tokenizer(line_buf, &line_pos, buf);
      if ((found_v = (int*) hashtable_search(ht_speciesName2Id, buf)) == NULL){
        printf("%s, %s\n", fileName, buf);
        printf("There is no value for input key! - species hash_t\n");
        exit(-1);
      } else{
        (data->clusters[i]).sequences[j].species_id = *found_v;
      }

      // get a seed score---------------------
      line_tokenizer(line_buf, &line_pos, buf);
      (data->clusters[i]).sequences[j].seed = atof(buf);

      // get a sequence name------------------
      line_tokenizer(line_buf, &line_pos, buf);

      // if new
      if ((found_v = (int*) hashtable_search(ht_seqName2Id, buf)) == NULL){
        // make a key and a value
        k = (h_key_t*) malloc ((strlen(buf) +1)* sizeof(h_key_t));
        strcpy(k, buf);
        v = (h_value_t*) malloc (sizeof(h_value_t));
        *v  = cur_sequence_id;
        // add them to hash table
        hashtable_insert(ht_seqName2Id, k, v);
        temp_sequence_id = cur_sequence_id;
        // add it to sequence name table
        //sequence_names[cur_sequence_id] = (char*) malloc ((strlen(buf) + 1)* sizeof(char));
        strcpy(sequence_names[cur_sequence_id], buf);
        //sequence_names[cur_sequence_id] = k;
        cur_sequence_id++;
      }
      else
        temp_sequence_id = *found_v;
      

      (data->clusters[i]).sequences[j].sequence_id = temp_sequence_id;

      // don't read the rest of the line (e.g. bootstrap value)
    }
  }

  free(fileName);
  fclose(input);
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


