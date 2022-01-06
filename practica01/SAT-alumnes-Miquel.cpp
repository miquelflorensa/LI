#include <iostream>
#include <stdlib.h>
#include <algorithm>
#include <vector>
using namespace std;

#define UNDEF -1
#define TRUE 1
#define FALSE 0

uint numVars;
uint numClauses;
vector<vector<int> > clauses;
vector<int> model;
vector<int> modelStack;
uint indexOfNextLitToPropagate;
uint decisionLevel;

vector<vector<int> > negatius;
vector<vector<int> > positius;
vector<pair<int,int>> apareixenca;
vector<int> posicio;
int conflictes;


struct sort_pair {
  bool operator()(pair<int,int> &a, pair<int,int> &b) {
    return a.second > b.second;
  }
};

void readClauses(){
  // Skip comments
  char c = cin.get();
  while (c == 'c') {
    while (c != '\n') c = cin.get();
    c = cin.get();
  }  
  // Read "cnf numVars numClauses"
  string aux;
  cin >> aux >> numVars >> numClauses;
  clauses.resize(numClauses);  
  negatius.resize(numVars+1);
  positius.resize(numVars+1);
  apareixenca.resize(numVars+1, make_pair(0,0));
  posicio.resize(numVars+1);
  // Read clauses
  for (uint i = 0; i < numClauses; ++i) {
    int lit;
    while (cin >> lit and lit != 0) {
      clauses[i].push_back(lit);
      if (lit <= 0) {
        negatius[-lit].push_back(i);
      }
      else {
        positius[lit].push_back(i);
      }
    }
  }
  for (uint i = 0; i < numVars; ++i) {
    apareixenca[i] = make_pair(i,((positius[i].size())*(negatius[i].size())));
  }
  sort(apareixenca.begin(), apareixenca.end(), sort_pair());   

  for (uint i = 0; i < numVars; ++i) {
    posicio[apareixenca[i].first] = i; 
    //e.g. posicio del literal 45? és la pos 2
  }  
}



int currentValueInModel(int lit){
  if (lit >= 0) return model[lit];
  else {
    if (model[-lit] == UNDEF) return UNDEF;
    else return 1 - model[-lit];
  }
}


void setLiteralToTrue(int lit){
  modelStack.push_back(lit);
  if (lit > 0) model[lit] = TRUE;
  else model[-lit] = FALSE;   
}


bool propagateGivesConflict ( ) {
  while ( indexOfNextLitToPropagate < modelStack.size() ) {
    /*
    ++indexOfNextLitToPropagate;
    for (uint i = 0; i < numClauses; ++i) {
      bool someLitTrue = false;
      int numUndefs = 0;
      int lastLitUndef = 0;
      for (uint k = 0; not someLitTrue and k < clauses[i].size(); ++k){
       int val = currentValueInModel(clauses[i][k]);
       if (val == TRUE) someLitTrue = true;
       else if (val == UNDEF){ ++numUndefs; lastLitUndef = clauses[i][k]; }
     }
      if (not someLitTrue and numUndefs == 0) return true; // conflict! all lits false
      else if (not someLitTrue and numUndefs == 1) setLiteralToTrue(lastLitUndef);  
    }
    */

    int literal = modelStack[indexOfNextLitToPropagate];
    ++indexOfNextLitToPropagate;
    vector<int>* occur;
    if (literal > 0) occur = &negatius[literal];
    else occur = &positius[-literal]; 

    for (uint i = 0; i < occur->size(); ++i) {
      bool someLitTrue = false;
      int numUndefs = 0;
      int lastLitUndef = 0;

      for (uint k = 0; not someLitTrue and k < clauses[(*occur)[i]].size(); ++k) {
        int val = currentValueInModel(clauses[(*occur)[i]][k]);
        if (val == TRUE) someLitTrue = true;
        else if (val == UNDEF){ 
          ++numUndefs; 
          lastLitUndef = clauses[(*occur)[i]][k];
        }
      }
      if (not someLitTrue and numUndefs == 0) {
        ++conflictes;
        if (conflictes%1000 == 0) {
          for (uint i = 0; i < numVars; ++i) apareixenca[i].second /= 2;
        }
        for (uint k = 0; k < clauses[(*occur)[i]].size(); ++k) {
          int val = clauses[(*occur)[i]][k];
          apareixenca[posicio[val]].second += 10;
          while (posicio[val] > 0 and (apareixenca[posicio[val]].second > apareixenca[posicio[lastLitUndef]-1].second)) {
            int aux = posicio[apareixenca[posicio[val]-1].first];
            posicio[apareixenca[posicio[val]-1].first] = posicio[val];
            posicio[val] = aux;
          }
        }
        return true; // conflict! all lits false
      }
      else if (not someLitTrue and numUndefs == 1) setLiteralToTrue(lastLitUndef);
    }
  }
  return false;
}


void backtrack(){
  uint i = modelStack.size() -1;
  int lit = 0;
  while (modelStack[i] != 0){ // 0 is the DL mark
    lit = modelStack[i];
    model[abs(lit)] = UNDEF;
    modelStack.pop_back();
    --i;
  }
  // at this point, lit is the last decision
  modelStack.pop_back(); // remove the DL mark
  --decisionLevel;
  indexOfNextLitToPropagate = modelStack.size();
  setLiteralToTrue(-lit);  // reverse last decision
}


// Heuristic for finding the next decision literal:
int getNextDecisionLiteral(){
  for (uint i = 1; i <= numVars; ++i) 
    if (currentValueInModel(apareixenca[i].first) == UNDEF) return apareixenca[i].first;  // returns first UNDEF var, positively
  return 0; // reurns 0 when all literals are defined
}

void checkmodel(){
  for (uint i = 0; i < numClauses; ++i){
    bool someTrue = false;
    for (uint j = 0; not someTrue and j < clauses[i].size(); ++j)
      someTrue = (currentValueInModel(clauses[i][j]) == TRUE);
    if (not someTrue) {
      cout << "Error in model, clause is not satisfied:";
      for (uint j = 0; j < clauses[i].size(); ++j) cout << clauses[i][j] << " ";
        cout << endl;
      exit(1);
    }
  }  
}

int main(){ 
  readClauses(); // reads numVars, numClauses and clauses
  model.resize(numVars+1,UNDEF);
  indexOfNextLitToPropagate = 0;  
  decisionLevel = 0;
  conflictes = 0;
  
  // Take care of initial unit clauses, if any
  for (uint i = 0; i < numClauses; ++i)
    if (clauses[i].size() == 1) {
      int lit = clauses[i][0];
      int val = currentValueInModel(lit);
      if (val == FALSE) {cout << "UNSATISFIABLE" << endl; return 10;}
      else if (val == UNDEF) setLiteralToTrue(lit);
    }

  // DPLL algorithm
    while (true) {
      while ( propagateGivesConflict() ) {
        if ( decisionLevel == 0) { cout << "UNSATISFIABLE " << conflictes << endl; return 10; }
        backtrack();
      }
      int decisionLit = getNextDecisionLiteral();
      if (decisionLit == 0) { checkmodel(); cout << "SATISFIABLE " << conflictes << endl; return 20; }
    // start new decision level:
    modelStack.push_back(0);  // push mark indicating new DL
    ++indexOfNextLitToPropagate;
    ++decisionLevel;
    setLiteralToTrue(decisionLit);    // now push decisionLit on top of the mark
  }
}  
