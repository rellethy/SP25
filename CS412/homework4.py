def apriori(transaction_filename, cost_filename, s, k, m):
    """
    transaction_filename: the name of the file containing the transactions
    cost_filename: the name of the file containing the costs
    
    s: the minimum support threshold
    k: the maximum sum of the costs of the items in the itemset
    m: the minimum price of the itemset
    """
from itertools import combinations
from collections import defaultdict
# need to read in transaction file need to split up each line in file, should 
# convert into a sorted list and then return that list 
def load_transactions(transaction_file):
        #need to open file using read, we'll call it f
        with open(transaction_file, 'r') as f:
            #need to clean up the transaction file
            #will have to strip the delimiter and then sort the items 
            transactions = []
            for line in f:
                 line = line.strip() # remove white space
                 if line: #gotta check if theres anything left on the line
                      items = list(line) #will use a list of chars and then sort
                      items.sort()
                      transactions.append(items)
            return transactions
        
#this file maps each item to a cost 
#need to read in and store in a dictionary the item name and its cost 
def load_costs(cost_file): 
    cost_dict = {}
    with open(cost_file, 'r') as f: 
         for line in f: 
            line = line.strip()
            if line: # check if theres stuff left 
                 item, cost = line.strip().split(',') #should be comma separated
                 cost_dict[item] = int(cost)
    return cost_dict
                

def get_support(transactions, itemset):
    support = 0

    for transaction in transactions:
         #need to convert the transaction and itemset into sets
         transaction_set = set(transaction)
         set_of_itemsets = set(itemset)     

         #look at each itemset is entirely in the transactionset
         #if its not we know its not a subset of the transaction set
         if set_of_itemsets.issubset(transaction_set):
              support += 1
    
    return support

def valid_constraints(itemset, cost_dict, k, m):
    #need to reference the price dict for the cost of each item
    # only continue if all items exist in the cost dictionary
    if not all(i in cost_dict for i in itemset):
        return False  # if even one item is missing a price, reject it

    prices = [cost_dict[i] for i in itemset]
    return sum(prices) <= k and min(prices) >= m
        

#take in the previous frequent itemset 
#trying to generate the candidates for the next scan

def generate_candidates(prev_frequent, size):
    candidates = set()
     #sort so we can compare prefixes and cut dupes
    items = sorted(prev_frequent.keys())
     
     #need to compare all the unique pairs of itemsets in the previous 
     #frequent set
    for i in range(len(items)):
        for j in range(i + 1, len(items)):
            #what are the conditions to join elements of the frequent set
            # well the first k - 2 items must mach 
            # AB and AC must share A 
            # we need to make sure we dont double count too 
            a, b = items[i], items[j]
            if a[:size - 2] == b[:size - 2] and a[size - 2] < b[size - 2]:
                #this should make all joins valid, with no dupes
                #condition checks out so we can make new candidate
                new_candidate = ''.join(sorted(set(a) | set(b)))
                #now we can prune
                #we need to make sure every k-1 subset of the candiadte exists
                #in the previous frequent set
                #this is huge because we can prune a lot of candidates out immediately
                #my baby itertool's combinations function will generate all the k - 1 itemsets
                subsets = combinations(new_candidate, size - 1) 
                #valid if they made it through pruning 
                is_valid = True 
                for sub in subsets:
                    sorted_subset = ''.join(sorted(sub))
                    if sorted_subset not in prev_frequent:
                        is_vaild = False #if any subsets missing we prune
                        break #move on
                    #we finally got a candidate thats worth looking at
                if is_valid:
                    candidates.add(new_candidate)
    return candidates

#the big bucks 
#takes in transaction_file, cost file, support threshold, max sum of prices, min item price 
def apriori(transaction_filename, cost_filename, s, k, m):
    #load up 
    transactions = load_transactions(transaction_filename)
    cost_dict = load_costs(cost_filename)

    #should have a list of each tranasaction, each element of this list is a separate transaction
    #each transaction has letters signaling different items 
    #cost dict tells us what each item's price was 

    apriori_result = {}
    #what scan are we on???
    scan = 1

    #starting at C1, we need to store each item and update their counts 
    #RAW SUPPORT COUNTS WE NOT DOING NO FILTERING YET
    item_counts = defaultdict(int)
    for transaction in transactions:
        for item in transaction:
            item_counts[item] += 1

    #for each itemlist in C1, we check our baseline constraints 
    C1 = {}
    for item, count in item_counts.items():
        if valid_constraints([item], cost_dict, k, m):
            C1[item] = count
    

    #first frequent itemset pass 
    #store each itemset if they pass minsupport threshold (dont need to call get_support because we just counted this first time)
    F1 = {item: count for item, count in C1.items() if count >= s}
    if not F1: 
        # if our frequent item list is empty, we're done!! 
        # wouldnt this be nice
        return apriori_result
    
    #store C1 and F1 in result dictionary, the whole frequent set should be under pass 1
    apriori_result[scan] = {'c' : C1, 'f' : F1}
    # of course we need our previous frequent set if we lose this we're cooked
    prev_frequent = F1.copy()

    #now we can iterate and scan until we reach our closed frequent itemsets
    while True: 
        #update for scan k 
        scan += 1
        #we need to keep track of how many items are in each candidate itemset now
        size = scan

        #now we can pull the candidates from the previous itemset 
        candidates = generate_candidates(prev_frequent, size)

        #we're done if ther arent any valid combinatons
        if not candidates:
            break
        
        #now we can work with C2 C3... CK
        #for every candidate we need to check the price constraints, if valid we get teh support count 
        #the only way we dont store a candidate in CK is if the support is 0 
        CK = {}
        for candidate in candidates: 
            itemset = list(candidate)
            if valid_constraints(itemset, cost_dict, k, m):
                support = get_support(transactions, itemset)
                if support > 0: 
                    CK[''.join(sorted(itemset))] = support 
        #need to make sure candidates isnt empy
        
        #need now our frequent itemset dict
        FK = {item: sup for item, sup in CK.items() if sup >= s}
        apriori_result[scan] = {'c': CK, 'f': FK}
        # Break *after* storing if frequent set is empt
        if not FK:
            break

        #now we can scan and save the result
        prev_frequent = FK

        #we can stop if there is only one itemset left
        if len(FK) == 1: 
            break
    
    return apriori_result

