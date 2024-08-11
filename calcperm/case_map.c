//////
// Intro

// Switch on string

// (c) 2011..2020 Sigrid/Jasper Neumann
// www.sirrida.de / programming.sirrida.de
// E-Mail: info@sirrida.de

// Granted to the public domain
// First version: 2013-02-23
// Last change: 2013-04-12

// Hash via optimized map.
// gcc

// Options:
//   DEBUG_CASE_DUPE: Check for dupes at runtime: Output an error message.
//   DEBUG_CASE_STATISTICS: Dump mapping statistics
//   USE_MEMCMP: Use memcmp instead of strcmp
//   CHECK_IDENTITY: Check string constants for identity beforehand

#include "case_map.h"

mycall t_int floor_power_2(t_int x) {
// Round down to next power of 2
  t_int y;

  do {
    y = x;
    x = x & (x-1);
    } while (x!=0);
  return y;
  }

mycall t_int ceil_power_2(t_int x) {
// Round up to next power of 2

  return floor_power_2(x-1) << 1;
  }

mycall static t_int hash_0(UNUSED(const t_char* s)) {
  return 0;
  }

mycall static t_int hash_1(const t_char* s) {
  return s[0];
  }

mycall static t_int hash_2(const t_char* s) {
  if (s[0]==0) {
    return 0;
    }
  else {
    return s[0]*37+s[1];
    }
  }

mycall static t_int hash_3(const t_char* s) {
  if (s[0]==0) {
    return 0;
    }
  else if (s[1]==0) {
    return s[0];
    }
  else if (s[2]==0) {
    return s[0]*139+s[1];
    }
  else {
    return s[0]*139+s[1]*37+s[2];
    }
  }

mycall static t_int hash_4(const t_char* s) {
  if (s[0]==0) {
    return 0;
    }
  else if (s[1]==0) {
    return s[0];
    }
  else if (s[2]==0) {
    return s[0]*139+s[1];
    }
  else if (s[3]==0) {
    return s[0]*139+s[1]*37+s[2];
    }
  else {
    return s[0]*139+s[1]*37+s[2]*11+s[3];
    }
  }

mycall static t_int hash_5(const t_char* s) {
  t_int res;

  res=0;
  while (*s!=0) {
    res = res + *s;
    ++s;
    }
  return res;
  }

mycall static t_int hash_6(const t_char* s) {
  t_int res;

  res=0;
  while (*s!=0) {
    res = res*5 + *s;
    ++s;
    }
  return res;
  }

mycall static t_int hash_7(const t_char* s) {
  t_int res;

  res=0;
  while (*s!=0) {
    res = res*37 ^ *s;
    ++s;
    }
  return res;
  }

mycall static t_int hash_8(const t_char* s) {
  t_int res;

  res=0;
  while (*s!=0) {
    res = res*43 ^ *s;
    ++s;
    }
  return res;
  }

mycall static t_int hash_9(const t_char* s) {
  t_int res;

  res=0;
  while (*s!=0) {
    res = ((res*0x4555) >> 8) ^ *s;
    ++s;
    }
  return res;
  }

// Times measured on Intel i7 920 / 32 bit / GCC 4.7.2 -O3
#define string_compare_cost 60.0
  // Assumed cyles per string compare call incl. loop and call overhead.

static const struct {
  mycall t_int (*hash)(const t_char* s);
  float cost;  // Assumed cyles per execution of a hash function
  } a_hash[] = {
  {hash_0,   4.0},
  {hash_1,   4.0},
  {hash_2,   6.0},
  {hash_3,  20.0},
  {hash_4,  25.0},
  {hash_5,  82.0},
  {hash_6,  87.0},
  {hash_7,  96.0},
  {hash_8, 113.0},
  {hash_9, 160.0},
  {0,        0.0} };  // Sentinel

mycall t_bool string_map_init(tr_string_map* _m_) {
// result: already finalized?

  SYNC_SYNCHRONIZE_LOAD
  GLOBAL_MUTEX_WAIT
  if (_m_->finalized) {
    GLOBAL_MUTEX_SIGNAL
    return true;
    }
  else {
    _m_->nr_slots = 1;
    _m_->mask = 0;
    _m_->map = (tr_string_entry**) calloc(1, sizeof(tr_string_entry*));
    return false;
    }
  }

mycall void string_map_finalize(tr_string_map* _m_) {
  t_int count;
  tr_string_entry* p;
  float cost;
  float best_cost;
  long sum2;
  t_int* a_count;
  t_int q;
  t_int i;
  t_int idx;
  tr_string_entry** new_map;
  tr_string_entry* p1;
  t_int slot;

  p = _m_->map[0];
  count = 0;
  while (p != 0) {
    ++count;
#ifdef USE_MEMCMP
    p->len_1 = strlen(p->s)+1;  // length+1, i.e. including 0 character
#endif
    p = p->next;
    }
  if (count<=1) {
    // One slot and a trivial hash function is sufficient for a single string.
    #ifdef DEBUG_CASE_STATISTICS
      printf("Mapping statistics: count=%i, #slots=1, trivial case\n", count);
    #endif
    _m_->hash = &hash_0;
    }
  else {
    // count must be >0 here.
    _m_->nr_slots = ceil_power_2(count);  // Load factor 0.5..1.0.
    _m_->mask = _m_->nr_slots-1;  // _m_->nr_slots is a power of 2.

    // Find the best hash function.
    a_count = (t_int*) malloc(_m_->nr_slots*sizeof(t_int));
    #ifdef DEBUG_CASE_STATISTICS
      printf("Mapping statistics: count=%i, #slots=%i; expected #cmp=%f\n",
        count, _m_->nr_slots, (1.0+0.5*(float)count/(float)_m_->nr_slots) );
    #endif
    idx = 0;
    best_cost = 0;  // dummy
    for (q=0; a_hash[q].hash; ++q) {
      // Simulate rehashing in order to determine the slot "lengths".
      #ifdef DEBUG_CASE_STATISTICS
        printf("Hash #%i: ", q);
      #endif
      for (i=0; i<_m_->nr_slots; ++i) {
        a_count[i] = 0;
        }
      p = _m_->map[0];
      while (p != 0) {
        slot = a_hash[q].hash(p->s) & _m_->mask;
        ++a_count[slot];
        p = p->next;
        }

      // Calculate the number of needed calls to cmpstr
      // needed to look up all given strings.
      sum2 = 0;
      for (i=0; i<_m_->nr_slots; ++i) {
        #ifdef DEBUG_CASE_STATISTICS
          printf("%i ", a_count[i]);
        #endif
        sum2 += ((long)a_count[i]*((long)a_count[i]+1)) >> 1;
        }
      // Calculate mean cost.
      cost = ((float)sum2/(float)count)*string_compare_cost+a_hash[q].cost;
      #ifdef DEBUG_CASE_STATISTICS
        // printf("=> #cmp=%li, cost=%f\n", sum2, cost);
        printf("=> #cmp=%f, cost=%f\n", (float)sum2/(float)count, cost);
      #endif
      if ((q==0) || (cost<best_cost)) {
        idx = q;
        best_cost = cost;
        }
      }

    _m_->hash = a_hash[idx].hash;
    #ifdef DEBUG_CASE_STATISTICS
      printf("Best hash: #%i, cost=%f\n", idx, best_cost);
    #endif

    free(a_count);

    // Rehash.
    new_map = (tr_string_entry**) calloc(_m_->nr_slots, sizeof(tr_string_entry*));
    p = _m_->map[0];  // This is a simplified rehash, we only have this one slot.
    free(_m_->map);
    _m_->map = new_map;
    while (p != 0) {
      slot = _m_->hash(p->s) & _m_->mask;
      // printf("slot %i: %s\n", slot,p->s);
      #ifdef DEBUG_CASE_DUPE
        if (string_map_find(_m_, p->s, -1) != -1) {
          printf("DUPE OF CASE %s\n", p->s);
          }
      #endif
      p1 = p->next;
      p->next = new_map[slot];
      new_map[slot] = p;
      p = p1;
      }

    }
  SYNC_SYNCHRONIZE_STORE
  _m_->finalized = true;
  GLOBAL_MUTEX_SIGNAL
  }

mycall t_int string_map_find(tr_string_map* _m_, const t_char* s, t_int not_found) {
  t_int slot;
  tr_string_entry* p;

  SYNC_SYNCHRONIZE_LOAD
  slot = _m_->hash(s) & _m_->mask;
  p = _m_->map[slot];
  while (p != 0) {
#ifdef CHECK_IDENTITY
    // Optional check for constant identity
    if (p->s == s) {
      return p->i;
      }
#endif
#ifdef USE_MEMCMP
    // Replacement by memcmp; this is possible since we know one length
    if (memcmp((void*)p->s, (void*)s, p->len_1*sizeof(t_char)) == 0) {
      return p->i;
      }
#else
    // Classical approach via strcmp
    if (strcmp(p->s,s) == 0) {
      return p->i;
      }
#endif
    p = p->next;
    }
  return not_found;
  }

// eof.
