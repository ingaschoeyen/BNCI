# Plan for Assignment 2

## Structure Learning Algorithm

### 1. Hill Climbing Algorithm

#### 1.1. Description

Hill Climbing is a simple algorithm that starts with an empty network and adds/removes edges one at a time to maximize the score of the network. The score of the network is calculated using a scoring function that evaluates the network structure based on the data.

#### 1.2. Pseudocode

```python

def hill_climbing(data, max_parents, scoring_function):
    network = empty_network()
    best_score = -inf
    while True:
        neighbors = get_neighbors(network)
        for neighbor in neighbors:
            score = scoring_function(neighbor, data)
            if score > best_score:
                best_score = score
                network = neighbor
        if best_score == network_score(network):
            break
    return network

```

#### 1.3. Complexity

The complexity of the Hill Climbing algorithm depends on the number of possible edges in the network and the scoring function used. In the worst case, the algorithm can have exponential complexity, but in practice, it is usually much faster.

### 2. Greedy Search Algorithm

#### 2.1. Description

Greedy Search is a simple algorithm that starts with an empty network and adds/removes edges one at a time to maximize the score of the network. The score of the network is calculated using a scoring function that evaluates the network structure based on the data.

#### 2.2. Pseudocode

```python

def greedy_search(data, max_parents, scoring_function):
    network = empty_network()
    best_score = -inf
    while True:
        neighbors = get_neighbors(network)
        for neighbor in neighbors:
            score = scoring_function(neighbor, data)
            if score > best_score:
                best_score = score
                network = neighbor
        if best_score == network_score(network):
            break
    return network

```

#### 2.3. Complexity

The complexity of the Greedy Search algorithm depends on the number of possible edges in the network and the scoring function used. In the worst case, the algorithm can have exponential complexity, but in practice, it is usually much faster.

## Parameter Learning Algorithm

### 1. Maximum Likelihood Estimation

## 2 types of Causal Inference
