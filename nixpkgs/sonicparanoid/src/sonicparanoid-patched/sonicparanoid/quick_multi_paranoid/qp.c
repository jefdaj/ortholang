/* Copyright (c) 2008 
   Taekyung Kim, Jinha Kim, Minyoung Son, Hyeonseung Im, Sungwoo Park    
   Programming Language Laboratory, POSTECH
   gla@postech.ac.kr */

#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include "qp.h"
#include INTERMEDIATE_HEADER_FILE
#include "hashtable.h"
#include "hashtable_itr.h"

#define ASSERT(cond,msg)			\
  do{ \
    if (!(cond)) \
      { \
	printf("assert:%s\n",msg); \
	exit(-1); \
      } \
  } while (0)

// cluster_output_arr-----------------------------
typedef struct{
  int num_of_output_clusters;
  cluster_output* outputs;
} cluster_output_arr;

// seq_con_index
typedef struct
{
  int num_cons[NUM_OF_SEQUENCES];
  int* cons[NUM_OF_SEQUENCES];
} seq_con_index;

// seq_index
typedef struct
{
  sequence seqs[NUM_OF_SEQUENCES];
} seq_index;

// int_queue
// invariant: data should be positive
typedef struct
{
  int capacity;
  int len;
  int head_idx;
  int tail_idx;
  int *data;
  int *idx;
} int_queue;

typedef struct
{
  int num_of_links;
  sequence** start;
  sequence** end;
} link;

typedef int key;
typedef sequence* value;
typedef struct hashtable value_ht;
typedef int value_dummy;

seq_con_index* makeSeqConIndex();
int getSeqConIndex(seq_con_index* s, int seq1, int seq2);
void setSeqConIndex(seq_con_index* s, int seq1, int seq2);
void resetSeqConsOfASeq(seq_con_index* s, int seq);
int* getSeqConsFromASeq(seq_con_index* s, int seq);
int getNumSeqConFromASeq(seq_con_index* s, int seq);
void freeSeqConIndex(seq_con_index *s);
seq_index* makeSeqIndex();
sequence* getSeqIndex(seq_index* s, int seq_id);
void setSeqIndex(seq_index* s, sequence seq);
void freeSeqIndex(seq_index *s);
int getDataIdx(int s1, int s2);
void mergeCluster(seq_con_index* con_idx, seq_index* s_idx, int c1_s, int c1_len, int c2_s, int c2_len);
cluster_output_arr* makeOutputClusterFromIndex(seq_con_index* con_idx, seq_index* s_idx);
void getEdgesFromTwoSpecies(seq_con_index* con_idx, seq_index* s_idx, int s1, int s2);
void getEdgesFromSeedCluster(seq_con_index* con_idx, seq_index* s_idx, cluster* c, int s1, int s2);

int_queue* makeIntQueue(int init_size);
void freeIntQueue(int_queue* q);
int deIntQueue(int_queue* q);
void enIntQueue(int_queue* q, int i);

link* makeLink();
void freeLink(link* l);
void addLink(link* l, sequence* seq1, sequence* seq2);

int checkConflict(link* links); //myson

void makeOutputCluster(cluster_output_arr* output_arr, sequence** local_seqs, int local_seqs_num, int conflict);
void freeClusterOutputArr(cluster_output_arr *c);

int main(int argc, char **argv)
{
  int i,j;
  cluster_output_arr *output_arr = NULL;
  seq_con_index *con_idx = NULL;
  seq_index *seq_idx = NULL;
  
  con_idx = makeSeqConIndex();
  seq_idx = makeSeqIndex();

  for(i = 2; i < NUM_OF_SPECIES*2; i *=2 )
    {
      for( j = 0; j < NUM_OF_SPECIES; j += i)
	{
	  mergeCluster(con_idx, seq_idx, j,i/2,j+i/2,i/2);
	}
    }
  
  output_arr = makeOutputClusterFromIndex(con_idx,seq_idx);
  output_result(output_arr->num_of_output_clusters,output_arr->outputs);

  freeSeqConIndex(con_idx);
  freeSeqIndex(seq_idx);
  free_hashtables();
  freeClusterOutputArr(output_arr);

  return 0;
}

void mergeCluster(seq_con_index* con_idx, seq_index* s_idx, int c1_s, int c1_len, int c2_s, int c2_len)
{
  int i,j;

  for(i = 0; i <c1_len & c1_s+i < NUM_OF_SPECIES; i++)
    {
      for(j = 0; j < c2_len & c2_s+j < NUM_OF_SPECIES; j++ )
	{
	  getEdgesFromTwoSpecies(con_idx,s_idx,c1_s+i,c2_s+j);
	}
    }
}

void getEdgesFromTwoSpecies(seq_con_index* con_idx,seq_index* s_idx, int s1, int s2)
{
  int data_idx;
  dataFile* d = NULL;
  int i;

  d = (dataFile*)malloc(sizeof(dataFile));
  //    fprintf(stderr,"species %d, species %d\n",s1,s2);
  load_dataFile(s1,s2,d);

  for(i = 0; i < d->num_of_clusters; i++)
    {
      getEdgesFromSeedCluster(con_idx, s_idx, &((d->clusters)[i]),s1,s2);
    }
  free_dataFile(d);
}
void getEdgesFromSeedCluster(seq_con_index* con_idx, seq_index* s_idx, cluster* c, int s1, int s2)
{
  sequence** s1_seq;
  sequence** s2_seq;
  int s1_seq_num = 0;
  int s2_seq_num = 0;
  int i,j;

  s1_seq = (sequence**)malloc(sizeof(sequence*)*(c->num_of_sequences));
  ASSERT(s1_seq != NULL, "not enough memory");
  s2_seq = (sequence**)malloc(sizeof(sequence*)*(c->num_of_sequences));
  ASSERT(s2_seq != NULL, "not enough memory");

  for(i = 0, s1_seq_num = 0, s2_seq_num = 0; i < c->num_of_sequences; i++ )
    {
      if((c->sequences)[i].species_id == s1)
	{
	  s1_seq[s1_seq_num] = &((c->sequences)[i]);
	  s1_seq_num++;
	}
      else if((c->sequences)[i].species_id == s2)
	{
	  s2_seq[s2_seq_num] = &((c->sequences)[i]);
	  s2_seq_num++;
	}
      else
	{
	  ASSERT(0, "this cannot happen");
	}
      setSeqIndex(s_idx,(c->sequences)[i]);
    }

  for(i = 0; i < s1_seq_num; i++ )
    {
      for(j = 0; j < s2_seq_num; j++ )
	{
	  setSeqConIndex(con_idx,(s1_seq[i])->sequence_id,(s2_seq[j])->sequence_id);
	  //  printf("%d->%d\n",(s1_seq[i])->sequence_id,(s2_seq[j])->sequence_id);
	}
    }
  free(s1_seq);
  free(s2_seq);
}

/*
  XXX hash funciton
*/
unsigned int _seq_hash_funtion(void *k)
{
  key* kk = (key*)k;
  return *kk;
}

int _seq_hash_eq_fun(void *k1, void *k2)
{
  key *kk1 = (key*)k1;
  key *kk2 = (key*)k2;

  if( *kk1 == *kk2 )
    {
      return 1;
    }
  else
    {
      return 0;
    }
}

cluster_output_arr* makeOutputClusterFromIndex(seq_con_index *con_idx, seq_index *s_idx)
{
  cluster_output_arr* ret = NULL;

  sequence** local_seq_idx = NULL;
  key *kk;
  value *v;

  sequence* seq = NULL;

  int seq1_id;
  int seq2_id;
  int_queue* queue = NULL;
  link* links = NULL;

  int i,j,k;
  int conflict;

  ret = (cluster_output_arr*)malloc(sizeof(cluster_output_arr));
  ret->num_of_output_clusters = 0;
  ret->outputs = NULL;


  for( i = 0; i < NUM_OF_SEQUENCES; i++ )
    {
      int* cons_arr;
      int cons_num;
      int cluster_seq_num = 0;

      //     if(i % 10000 == 0)
      //	fprintf(stderr,"processing seq %d\n",i);

      if( 0 == getNumSeqConFromASeq(con_idx,i))
	{
	  continue;
	}
      queue = makeIntQueue(NUM_OF_SEQUENCES/20);
      local_seq_idx = (sequence**)calloc(NUM_OF_SEQUENCES,sizeof(sequence*));
      links = makeLink();

      local_seq_idx[i] = getSeqIndex(s_idx,i);
      cluster_seq_num++;

      cons_num = getNumSeqConFromASeq(con_idx,i);
      cons_arr = getSeqConsFromASeq(con_idx,i);
      //      fprintf(stderr,"[%d]\n",cons_num);

      for( j = 0; j < cons_num; j++)
	{

	  addLink(links,getSeqIndex(s_idx,i),getSeqIndex(s_idx,cons_arr[j]));
	  if( local_seq_idx[cons_arr[j]] == NULL )
	    {
	      enIntQueue(queue,cons_arr[j]);
	      local_seq_idx[cons_arr[j]] = getSeqIndex(s_idx,cons_arr[j]);
	      cluster_seq_num++;
	    }
	}
      resetSeqConsOfASeq(con_idx,i);

      while( -1 != (k = deIntQueue(queue)))
	{
	  int _cons_num;
	  int* _cons_arr;
	  int l;

	  _cons_num = getNumSeqConFromASeq(con_idx,k);
	  _cons_arr = getSeqConsFromASeq(con_idx,k);
	  //  fprintf(stderr,"<%d>\n",_cons_num);
	  for(l = 0; l < _cons_num; l++)
	    {

	      addLink(links,getSeqIndex(s_idx,k),getSeqIndex(s_idx,_cons_arr[l]));
	      if( local_seq_idx[_cons_arr[l]] == NULL )
		{
		  enIntQueue(queue,_cons_arr[l]);
		  local_seq_idx[_cons_arr[l]] = getSeqIndex(s_idx,_cons_arr[l]);
		  cluster_seq_num++;
		}
	    }
	  resetSeqConsOfASeq(con_idx,k);
	}
      freeIntQueue(queue);
      //check conflict
      conflict = checkConflict(links);
      //make cluster
      makeOutputCluster(ret,local_seq_idx,cluster_seq_num,conflict);
      //free link
      freeLink(links);
      // 
      free(local_seq_idx);

    }

  return ret;
}

void freeClusterOutputArr(cluster_output_arr *c)
{
  int i;

  for(i = 0; i < c-> num_of_output_clusters; i++ )
    {
      free((c->outputs)[i].sequences);
    }

  free(c->outputs);
  free(c);
}

int getDataIdx(int s1, int s2)
{
  int n = NUM_OF_SPECIES;

  ASSERT( s1 < n && s2 < n, "invalid species indices");
  ASSERT( s1 < s2, "the second species id should be larger than the first one.");

  return ((n-1)*n/2) - ((n-s1-1)*(n-s1)/2) + (s2-s1-1);
}

seq_con_index* makeSeqConIndex()
{
  seq_con_index* ret = NULL;
  int i;

  ret = (seq_con_index*)malloc(sizeof(seq_con_index));
  ASSERT(ret != NULL, "not enough memory");

  for(i = 0; i < NUM_OF_SEQUENCES; i++ )
    {
      (ret->num_cons)[i] = 0;
      (ret->cons)[i] = NULL;
    }

  return ret;
}

int getSeqConIndex(seq_con_index* s, int seq1, int seq2)
{
  int* con_arr = (s->cons)[seq1];
  int num_con = (s->num_cons)[seq1];
  int i;

  for( i = 0; i < num_con; i++ )
    {
      if(con_arr[i] == seq2)
	{
	  con_arr[i] = -1; // XXX
	  return 1;
	}
    }
  return -1;
}

void resetSeqConsOfASeq(seq_con_index* s, int seq)
{
  if( (s->cons)[seq] != NULL )
    {
      free((s->cons)[seq]);
    }
  (s->cons)[seq] = NULL;
  (s->num_cons)[seq] = 0;
}
int* getSeqConsFromASeq(seq_con_index* s, int seq)
{
  return ((s->cons)[seq]);
}

int getNumSeqConFromASeq(seq_con_index* s, int seq)
{
  return ((s->num_cons)[seq]);
}

void setSeqConIndex(seq_con_index* s, int seq1, int seq2)
{
  int s1 = seq1;
  int s2 = seq2;

  ((s->num_cons)[s1])++;
  (s->cons)[s1] = (int*)realloc((s->cons)[s1],sizeof(int)*((s->num_cons)[s1]));
  (s->cons)[s1][(s->num_cons)[s1] - 1] = s2;

  ((s->num_cons)[s2])++;
  (s->cons)[s2] = (int*)realloc((s->cons)[s2],sizeof(int)*((s->num_cons)[s2]));
  (s->cons)[s2][(s->num_cons)[s2] - 1] = s1;


}

void freeSeqConIndex(seq_con_index *s)
{
  int i;

  for(i = 0; i < NUM_OF_SEQUENCES; i++ )
    {
      free((s->cons)[i]);
    }

  free(s);
} 

seq_index* makeSeqIndex()
{
  seq_index* ret = NULL;
  int i;

  ret = (seq_index*)malloc(sizeof(seq_index));
  ASSERT(ret != NULL, "not enough memory");

  /*   for(i = 0; i < NUM_OF_SEQUENCES; i++) */
  /*     { */
  /*       (ret->seqs)[i] = NULL; */
  /*     } */

  return ret;
}

sequence* getSeqIndex(seq_index* s, int seq)
{
  return (&((s->seqs)[seq]));
}

void setSeqIndex(seq_index* s, sequence seq)
{
  (s->seqs)[seq.sequence_id] = seq;
}

void freeSeqIndex(seq_index *s)
{
  free(s);
} 

int_queue* makeIntQueue(int init_capacity)
{
  int_queue *ret = NULL;
  
  ASSERT(init_capacity > 0, "initial capacity should be more than zero.");
  
  ret = (int_queue*)malloc(sizeof(int_queue));
  ASSERT(ret != NULL, "not enough memory");
 
  ret->capacity = init_capacity;
  ret->len = 0;
  ret->head_idx = -1;
  ret->tail_idx = -1;
  ret->data = (int*)malloc(init_capacity*sizeof(int));
  ret->idx = (int*)calloc(NUM_OF_SEQUENCES,sizeof(int));

  return ret;
}

void freeIntQueue(int_queue* q)
{
  free(q->idx);
  free(q->data);
  free(q);
}

int deIntQueue(int_queue* q)
{
  int ret;

  if(q->len == 0)
    {
      ret = -1;
    }
  else
    {
      ret = (q->data)[q->head_idx];
      (q->len)--;
      q->head_idx = (q->head_idx + 1) % q->capacity;
      (q->idx)[ret] = 0;
    }
  
  return ret;
}

#define QUEUE_INC_AMOUNT (NUM_OF_SEQUENCES/20)
void enIntQueue(int_queue* q, int d)
{
  ASSERT(d >= 0, "data should be positive");

  if( (q->idx)[d] != 0)
    {
      return;
    }
  if(q->capacity == q->len)
    {
      int *temp = (int*)malloc((q->head_idx)*sizeof(int));
      int i;
      ASSERT(temp != NULL, "not enough memory");
      
      for(i = 0; i <q->head_idx; i++)
	{
	  temp[i] = (q->data)[i];
	}
      for(i = q->head_idx; i< q->capacity; i++)
	{
	  (q->data)[i-(q->head_idx)] = (q->data)[i];
	}
      for(i = 0; i < q->head_idx ; i++)
	{
	  (q->data)[i+(q->len - q->head_idx)] = temp[i];
	}
      q->head_idx = 0;
      q->tail_idx = q->len - 1;
      q->capacity += QUEUE_INC_AMOUNT;
      
      q->data = (int*)realloc(q->data,(q->capacity)*sizeof(int));
      ASSERT(q->data != NULL,"not enough memory");

      free(temp);
    }

  if(q->len == 0)
    {
      q->head_idx = 0;
      q->tail_idx = 0;
      (q->data)[q->tail_idx] = d;
    }
  else
    {
      q->tail_idx = (q->tail_idx + 1) % q->capacity;
      (q->data)[q->tail_idx] = d;
    }
  (q->idx)[d] = 1;
  (q->len)++;

}

link* makeLink()
{
  link* ret = NULL;

  ret = (link*)malloc(sizeof(link));
  ret->num_of_links = 0;
  ret->start = NULL;
  ret->end = NULL;

  return ret;
}

void freeLink(link* l)
{
  free(l->start);
  free(l->end);
  free(l);
}

void addLink(link* l, sequence* seq1, sequence* seq2)
{
  (l->num_of_links)++;
  l->start = (sequence**)realloc(l->start,sizeof(sequence*)*(l->num_of_links));
  l->end = (sequence**)realloc(l->end,sizeof(sequence*)*(l->num_of_links));
  (l->start)[(l->num_of_links) -1] = seq1;
  (l->end)[(l->num_of_links) -1] = seq2;
}

void makeOutputCluster(cluster_output_arr* output_arr, sequence** h,int h_num, int conflict)
{
  cluster_output* c_output;
  struct hashtable_itr* h_itr;
  value* v;
  int i,j;

  //make one more room
  (output_arr->num_of_output_clusters)++;
  output_arr->outputs = (cluster_output*)realloc(output_arr->outputs,sizeof(cluster_output)*(output_arr->num_of_output_clusters));
  c_output = &((output_arr->outputs)[(output_arr->num_of_output_clusters)-1]);

  c_output->tree_conflict = conflict;
  c_output->num_of_sequences = h_num;
  c_output->sequences = (sequence*)malloc(sizeof(sequence)*(c_output->num_of_sequences));

  for(i = 0,j = 0; i < NUM_OF_SEQUENCES; i++ )
    {
      if( h[i] != NULL )
	{
	  (c_output->sequences)[j] = *(h[i]);
	  j++;
	}
    }
  
}

//========================================================================================
void addSeqToHt(sequence* start_seq, sequence* end_seq, struct hashtable* ht_species2ht){
  key* k = NULL;
  key* kk = NULL;
  key* kkk = NULL;
  value_ht* v_ht = NULL;
  value_ht* vv_ht = NULL;
  value_dummy* vvv_dummy = NULL;

  if ((v_ht = hashtable_search(ht_species2ht, &(start_seq->species_id))) == NULL){    // if a new species
    // step 1
    k = (key*) malloc(sizeof(key));
    *k = start_seq->species_id;
    v_ht = create_hashtable (10, _seq_hash_funtion, _seq_hash_eq_fun);

    // step 2
    kk = (key*) malloc(sizeof(key));
    *kk = end_seq->species_id;
    vv_ht = create_hashtable (10, _seq_hash_funtion, _seq_hash_eq_fun);

    // step 3
    kkk = (key*) malloc(sizeof(key));
    *kkk = start_seq->sequence_id;
    vvv_dummy = (value_dummy*) malloc(sizeof(value_dummy));
    *vvv_dummy = start_seq->sequence_id;

    hashtable_insert(vv_ht, kkk, vvv_dummy);
    hashtable_insert(v_ht, kk, vv_ht);
    hashtable_insert(ht_species2ht, k, v_ht);
  } else{ // if an existed species
    if ((vv_ht = hashtable_search(v_ht, &(end_seq->species_id))) == NULL ){    // if a new end_species
      // step2
      kk = (key*) malloc(sizeof(key));
      *kk = end_seq->species_id;
      vv_ht = create_hashtable (10, _seq_hash_funtion, _seq_hash_eq_fun);

      // step 3
      kkk = (key*) malloc(sizeof(key));
      *kkk = start_seq->sequence_id;
      vvv_dummy = (value_dummy*) malloc(sizeof(value_dummy));
      *vvv_dummy = start_seq->sequence_id;

      hashtable_insert(vv_ht, kkk, vvv_dummy);
      hashtable_insert(v_ht, kk, vv_ht);
    } else{ // if an existed sequence_id
      if ((vvv_dummy = hashtable_search(vv_ht, &(start_seq->sequence_id))) == NULL ){    // if a new start_sequence_id
        // step 3
        kkk = (key*) malloc(sizeof(key));
        *kkk = start_seq->sequence_id;
        vvv_dummy = (value_dummy*) malloc(sizeof(value_dummy));
        *vvv_dummy = start_seq->sequence_id;

        hashtable_insert(vv_ht, kkk, vvv_dummy);
      } else // if an existed start_sequence_id
        ;
    }
  }
}

//==============================================================================
int compareTwoHt(struct hashtable* first_vv_ht, struct hashtable* vv_ht){
  value_dummy* vvv_dummy = NULL;
  struct hashtable_itr* hhh_itr = NULL;

  // check diff numbers
  if (hashtable_count(first_vv_ht) != hashtable_count(vv_ht))
    return DIFF_NUMBERS;

  // check diff names
  hhh_itr = hashtable_iterator(vv_ht);
  if( hhh_itr == NULL || hashtable_count(vv_ht) == 0 )
    {
      return NO;
    }

  do{  // for each sequence - step 3
    vvv_dummy = hashtable_iterator_value(hhh_itr);
    if (hashtable_search(first_vv_ht, vvv_dummy) == NULL){
      return DIFF_NAMES;
    }
  } while( hashtable_iterator_advance(hhh_itr) != 0 );

  free(hhh_itr);
  return NO;
}

//==============================================================================
int findConflict(struct hashtable* v_ht){
  int conflict = NO;
  value_ht* vv_ht = NULL;
  value_ht* first_vv_ht = NULL;
  struct hashtable_itr* hh_itr = NULL;

  hh_itr = hashtable_iterator(v_ht);
  if( hh_itr == NULL || hashtable_count(v_ht) == 0 )
    {
      return conflict;
    }

  first_vv_ht = hashtable_iterator_value(hh_itr);
  // if #end.species_id = 1, ignore it. (It never causes conflict.)
  while( hashtable_iterator_advance(hh_itr) != 0){  // for each pair - step 2
    vv_ht = hashtable_iterator_value(hh_itr);
    conflict = compareTwoHt(first_vv_ht, vv_ht);
    if (conflict == DIFF_NAMES || conflict == DIFF_NUMBERS)
      break;
  };

  free(hh_itr);
  return conflict;
}

//==============================================================================
void destroyHashForConflict(struct hashtable* ht_species2ht){
  struct hashtable_itr* h_itr = NULL;
  struct hashtable_itr* hh_itr = NULL;
  int i = 0;

  value_ht* v_ht = NULL;
  value_ht* vv_ht = NULL;
    
  h_itr = hashtable_iterator(ht_species2ht);
  if( h_itr == NULL || hashtable_count(ht_species2ht) == 0 )
    return;

  do{ // for each species - step 1
    v_ht = hashtable_iterator_value(h_itr); 

    hh_itr = hashtable_iterator(v_ht);
    if( hh_itr == NULL || hashtable_count(v_ht) == 0)
      {
	hashtable_destroy(v_ht,0);
	continue;
      }

    do{ // for each pair - step 2
      vv_ht = hashtable_iterator_value(hh_itr); 
      hashtable_destroy(vv_ht, 1);
    } while( hashtable_iterator_advance(hh_itr) != 0);

    free(hh_itr);

    hashtable_destroy(v_ht, 0);
    i++;
  } while( hashtable_iterator_advance(h_itr) != 0);

  hashtable_destroy(ht_species2ht, 0);

  free(h_itr);
}

//==============================================================================
int sub_checkConflict(link* links, int start_species_id, int end_species_id){
  int i;
  // species_id -> ht (seq -> void)
  struct hashtable* ht_species2ht = NULL;
  struct hashtable_itr* h_itr = NULL;
  value_ht* v_ht = NULL;
  int conflict = NO;

  ht_species2ht = create_hashtable(10, _seq_hash_funtion, _seq_hash_eq_fun);

  // make a tree
  for (i = 0; i < links->num_of_links; i ++){
    // invariant: if start.species_id == end.species_id, we ignore the link (we do not add it).
    if ((links->start[i])->species_id != (links->end[i])->species_id){
      if ((links->start[i])->species_id >= start_species_id && (links->start[i])->species_id <= end_species_id)
        addSeqToHt(links->start[i], links->end[i], ht_species2ht);
      if ((links->end[i])->species_id >= start_species_id && (links->end[i])->species_id <= end_species_id)
        addSeqToHt(links->end[i], links->start[i], ht_species2ht);
    }
  }

  //if #species_id < 3, return NO.
  if (hashtable_count(ht_species2ht) < 3)
    conflict = NO;
  else{
    // find a conflict
    h_itr = hashtable_iterator(ht_species2ht);
    if( h_itr != NULL && hashtable_count(ht_species2ht) > 0)
      {
	do{ // for each species - step 1
	  v_ht = hashtable_iterator_value(h_itr); 
	  conflict = findConflict(v_ht);
	  if (conflict == DIFF_NAMES || conflict == DIFF_NUMBERS){
	    break;
	  }
	} while( hashtable_iterator_advance(h_itr) != 0);
      }
    free(h_itr);
  }

  // destroy hashtable
  destroyHashForConflict(ht_species2ht);

  return conflict;
}

//==============================================================================
int checkConflict(link* links){
  int threshold = 10000000;
  int interval;
  int conflict = NO;
  int start_species_id, end_species_id;

  // if #link < 3, return NO.
  if (links->num_of_links < 3)
    return NO;

  if (links->num_of_links < threshold){
    // check all species
    conflict = sub_checkConflict(links, 0, NUM_OF_SPECIES-1);
  } else{
    start_species_id = 0;
    end_species_id = 0;
    interval = NUM_OF_SPECIES/((links->num_of_links)/threshold + 1);

    while (start_species_id < NUM_OF_SPECIES){
      end_species_id = start_species_id + (interval - 1);
      if (end_species_id >= NUM_OF_SPECIES)
        end_species_id = NUM_OF_SPECIES - 1;

      //fprintf(stderr, "start %d, end %d\n", start_species_id, end_species_id);
      conflict = sub_checkConflict(links, start_species_id, end_species_id);

      if (conflict == DIFF_NUMBERS || conflict == DIFF_NAMES)
        break;
      start_species_id += interval;
    }
  }
  return conflict;
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


