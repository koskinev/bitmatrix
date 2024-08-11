//////
// Intro

// Switch on string

// (c) 2011..2020 Sigrid/Jasper Neumann
// www.sirrida.de / programming.sirrida.de
// E-Mail: info@sirrida.de

// Granted to the public domain
// First version: 2013-02-23
// Last change: 2013-04-12

// Hash via optimized map (STL strings).
// g++

// Options:
//   DEBUG_CASE_DUPE: Check for dupes at runtime: Output an error message.
//   DEBUG_CASE_STATISTICS: Dump a map statistics

#include "case_ms.hpp"

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

mycall static t_int hash_0(UNUSED(const string& s)) {
  return 0;
  }

mycall static t_int hash_1(const string& s) {
  return (s.empty()) ? 0 : s[0];
  }

mycall static t_int hash_2(const string& s) {
  switch (s.length()) {
    case  0: return 0;
    case  1: return s[0];
    default: return s[0]*37+s[1];
    }
  }

mycall static t_int hash_3(const string& s) {
  switch (s.length()) {
    case  0: return 0;
    case  1: return s[0];
    case  2: return s[0]*139+s[1];
    default: return s[0]*139+s[1]*37+s[2];
    }
  }

mycall static t_int hash_4(const string& s) {
  switch (s.length()) {
    case  0: return 0;
    case  1: return s[0];
    case  2: return s[0]*139+s[1];
    case  3: return s[0]*139+s[1]*37+s[2];
    default: return s[0]*139+s[1]*37+s[2]*11+s[3];
    }
  }

mycall static t_int hash_5(const string& s) {
  t_int res;
  t_int len;
  t_int i;

  res = 0;
  len = s.length();
  for (i=0; i<len; ++i) {
    res = res + s[i];
    }
  return res;
  }

mycall static t_int hash_6(const string& s) {
  t_int res;
  t_int len;
  t_int i;

  res = 0;
  len = s.length();
  for (i=0; i<len; ++i) {
    res = res*5 + s[i];
    }
  return res;
  }

mycall static t_int hash_7(const string& s) {
  t_int res;
  t_int len;
  t_int i;

  res = 0;
  len = s.length();
  for (i=0; i<len; ++i) {
    res = res*37 ^ s[i];
    }
  return res;
  }

mycall static t_int hash_8(const string& s) {
  t_int res;
  t_int len;
  t_int i;

  res = 0;
  len = s.length();
  for (i=0; i<len; ++i) {
    res = res*43 ^ s[i];
    }
  return res;
  }

mycall static t_int hash_9(const string& s) {
  t_int res;
  t_int len;
  t_int i;

  res = 0;
  len = s.length();
  for (i=0; i<len; ++i) {
    res = ((res*0x4555) >> 8) ^ s[i];
    }
  return res;
  }

mycall static t_int hash_a(const string& s) {
  return s.length();
  }

mycall static t_int hash_b(const string& s) {
  t_int len;

  len = s.length();
  if (len == 0) {
    return 0;
    }
  else {
    return len+s[0];
    }
  }

mycall static t_int hash_c(const string& s) {
  t_int len;

  len = s.length();
  if (len == 0) {
    return 0;
    }
  else {
    return len+s[len-1];
    }
  }

mycall static t_int hash_d(const string& s) {
  t_int len;

  len = s.length();
  if (len == 0) {
    return 0;
    }
  else {
    return len+s[0]+s[len-1];
    }
  }

mycall static t_int hash_e(const string& s) {
  t_int len;

  len = s.length();
  if (len == 0) {
    return 0;
    }
  else {
    return len*139+s[0]*37+s[len-1];
    }
  }

mycall static t_int hash_f(const string& s) {
  t_int len;

  len = s.length();
  if (len == 0) {
    return 0;
    }
  else {
    return len*139+s[0]*37+s[len-1]*11+s[len>>1];
    }
  }

// Times measured on Intel Atom N450 / 32 bit / GCC 4.7.2 -O3
#define string_compare_cost 225.0
  // Assumed cyles per string compare call incl. loop and call overhead.

static const struct {
  mycall t_int (*hash)(const string& s);
  float cost;  // Assumed cyles per execution of a hash function
  } a_hash[] = {
  {hash_0,   7.0},
  {hash_1,   8.0},
  {hash_2,  15.0},
  {hash_3,  28.0},
  {hash_4,  35.0},
  {hash_5,  84.0},
  {hash_6,  85.0},
  {hash_7,  97.0},
  {hash_8, 107.0},
  {hash_9, 243.0},
  {hash_a,   7.0},
  {hash_b,   8.0},
  {hash_c,   8.0},
  {hash_d,  10.0},
  {hash_e,  23.0},
  {hash_f,  31.0},
  {0,        0.0} };  // Sentinel

tr_string_map::tr_string_map() {
  this->hash = &hash_0;
  this->nr_slots = 0;
  this->mask = 0;
  this->map = 0;
  this->finalized = false;
  }

t_bool tr_string_map::init() {
// result: already finalized?

  SYNC_SYNCHRONIZE_LOAD
  GLOBAL_MUTEX_WAIT
  if (this->finalized) {
    GLOBAL_MUTEX_SIGNAL
    return true;
    }
  else {
    this->nr_slots = 1;
    this->mask = 0;
    this->map = (tr_string_entry**) calloc(1, sizeof(tr_string_entry*));
    return false;
    }
  }

void tr_string_map::finalize() {
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

  p = this->map[0];
  count = 0;
  while (p != 0) {
    ++count;
    p = p->next;
    }
  if (count<=1) {
    // One slot and a trivial hash function is sufficient for a single string.
    #ifdef DEBUG_CASE_STATISTICS
      cout << "Mapping statistics: count=" << count << ", #slots=1, trivial case\n";
    #endif
    this->hash = &hash_0;
    }
  else {
    // count must be >0 here.
    this->nr_slots = ceil_power_2(count);  // Load factor 0.5..1.0.
    this->mask = this->nr_slots-1;  // this->nr_slots is a power of 2.

    // Find best hash function.
    a_count = (t_int*) malloc(this->nr_slots*sizeof(t_int));
    #ifdef DEBUG_CASE_STATISTICS
      cout << "Mapping statistics: count=" << count
        << ", #slots=" << this->nr_slots
        << "; expected #cmp=" << (1.0+0.5*(float)count/(float)this->nr_slots)
        << "\n";
    #endif
    idx = 0;
    best_cost = 0;  // dummy
    for (q=0; a_hash[q].hash; ++q) {
      // Simulate rehashing in order to determine the slot "lengths".
      #ifdef DEBUG_CASE_STATISTICS
        cout << "Hash #" << q << ": ";
      #endif
      for (i=0; i<this->nr_slots; ++i) {
        a_count[i] = 0;
        }
      p = this->map[0];
      while (p != 0) {
        slot = a_hash[q].hash(p->s) & this->mask;
        ++a_count[slot];
        p = p->next;
        }

      // Calculate the number of needed calls to cmpstr
      // needed to look up all given strings.
      sum2 = 0;
      for (i=0; i<this->nr_slots; ++i) {
        #ifdef DEBUG_CASE_STATISTICS
          cout << a_count[i] << " ";
        #endif
        sum2 += ((long)a_count[i]*((long)a_count[i]+1)) >> 1;
        }
      // Calculate mean cost.
      cost = ((float)sum2/(float)count)*string_compare_cost+a_hash[q].cost;
      #ifdef DEBUG_CASE_STATISTICS
        // cout << "=> #cmp=" << sum2 << ", cost=" << cost << "\n";
        cout << "=> #cmp=" << (float)sum2/(float)count
             << ", cost=" << cost << "\n";
      #endif
      if ((q==0) || (cost<best_cost)) {
        idx = q;
        best_cost = cost;
        }
      }

    this->hash = a_hash[idx].hash;
    #ifdef DEBUG_CASE_STATISTICS
      cout << "Best hash: #" << idx << ", cost=" << best_cost << "\n";
    #endif

    free(a_count);

    // Rehash.
    new_map = (tr_string_entry**) calloc(this->nr_slots, sizeof(tr_string_entry*));
    p = this->map[0];  // This is a simplified rehash, we only have this one slot.
    free(this->map);
    this->map = new_map;
    while (p != 0) {
      slot = this->hash(p->s) & this->mask;
      // cout << "slot " << slot << ": " << p->s << "\n";
      #ifdef DEBUG_CASE_DUPE
        if (this->find(p->s, -1) != -1) {
          cout << "DUPE OF CASE " << p->s << "\n";
          }
      #endif
      p1 = p->next;
      p->next = new_map[slot];
      new_map[slot] = p;
      p = p1;
      }

    }
  SYNC_SYNCHRONIZE_STORE
  this->finalized = true;
  GLOBAL_MUTEX_SIGNAL
  }

t_int tr_string_map::find(const string& s, t_int not_found) {
  t_int slot;
  tr_string_entry* p;

  SYNC_SYNCHRONIZE_LOAD
  slot = this->hash(s) & this->mask;
  p = this->map[slot];
  while (p != 0) {
    if (p->s == s) {
      return p->i;
      }
    p = p->next;
    }
  return not_found;
  }

// eof.
