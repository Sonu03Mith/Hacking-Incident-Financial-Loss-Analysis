#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <chrono>
#include <iomanip>  // For setw for styling
#include "json.hpp" // To export results in JSON

using namespace std;
using json = nlohmann::json;

// --- SHARED TRANSACTION STRUCTURE ---
  struct Transaction
{
  string id;
  string timestamp;
  string sender_account;
  string reciver_amount;
  float amount;
  string transaction_type;
  string merchant_category;
  string location;
  string device_used;
  bool is_fraud;
  string fraud_type;
  double time_since_last_transaction;
  double spending_deviation_score;
  double velocity_score;
  double geo_anomaly_score;
  string payement_channel;
  string ip_address;
  string device_hash;
};


//==================================================================================
  // LINKED LIST IMPLEMENTATION
//==================================================================================
  
  namespace LinkedList {
    // Node structure for the Linked List
    struct Node
    {
      Transaction data;
      Node *next;
      Node(Transaction t) : data(t), next(nullptr) {}
      // Default constructor for dummy node
      Node() : next(nullptr) {}
    };
    
    struct ChannelLists
    {
      Node *card;
      Node *ach;
      Node *wire_transfer;
      Node *upi;
      
      ChannelLists() : card(nullptr), ach(nullptr), wire_transfer(nullptr), upi(nullptr) {}
    };
    
    // Insert at the head of the list
    void insert(Node *&head, Transaction t)
    {
      Node *newNode = new Node(t);
      newNode->next = head;
      head = newNode;
    }
    
    // Search by transaction type (linear search)
    void searchByTransactionType(Node *head, const string &type)
    {
      Node *current = head;
      int matchCount = 0;
      while (current != nullptr)
      {
        if (current->data.transaction_type == type)
        {
          matchCount++;
        }
        current = current->next;
      }
      if (matchCount == 0)
        cout << "No transactions found with type: " << type << endl;
      else
        cout << matchCount << " transactions found with type: " << type << endl;
    }
    
    // Helper to find the middle of the linked list
    Node *findMiddle(Node *head)
    {
      if (head == nullptr || head->next == nullptr) return head;
      Node *low = head, *high = head, *prev = nullptr;
      while (high != nullptr && high->next != nullptr)
      {
        prev = low;
        low = low->next;
        high = high->next->next;
      }
      if (prev) prev->next = nullptr;
      return low;
    }
    
    // Helper to merge two sorted linked lists
    Node *merge(Node *left, Node *right)
    {
      if (!left) return right;
      if (!right) return left;
      Node dummyNode;
      Node *tail = &dummyNode;
      while (left && right)
      {
        if (left->data.location <= right->data.location)
        {
          tail->next = left;
          left = left->next;
        }
        else
        {
          tail->next = right;
          right = right->next;
        }
        tail = tail->next;
      }
      tail->next = left ? left : right;
      return dummyNode.next;
    }
    
    // Merge sort for the linked list
    Node *mergeSortByLocation(Node *head)
    {
      if (head == nullptr || head->next == nullptr) return head;
      Node *middle = findMiddle(head);
      Node *left = head;
      Node *right = middle;
      
      left = mergeSortByLocation(left);
      right = mergeSortByLocation(right);
      
      return merge(left, right);
    }
    
    // Read CSV and populate the linked lists
    void readCSV(const string &filename, ChannelLists &channels)
    {
      ifstream file(filename);
      if (!file.is_open())
      {
        cout << "Error opening file!" << endl;
        return;
      }
      string line;
      getline(file, line); // Skip header
      while (getline(file, line))
      {
        stringstream ss(line);
        string token;
        Transaction t;
        getline(ss, t.id, ',');
        getline(ss, t.timestamp, ',');
        getline(ss, t.sender_account, ',');
        getline(ss, t.reciver_amount, ',');
        getline(ss, token, ',');
        t.amount = token.empty() ? 0.0f : stof(token);
        getline(ss, t.transaction_type, ',');
        getline(ss, t.merchant_category, ',');
        getline(ss, t.location, ',');
        getline(ss, t.device_used, ',');
        getline(ss, token, ',');
        t.is_fraud = (token == "1" || token == "true");
        getline(ss, t.fraud_type, ',');
        getline(ss, token, ',');
        t.time_since_last_transaction = token.empty() ? 0.0 : stod(token);
        getline(ss, token, ',');
        t.spending_deviation_score = token.empty() ? 0.0 : stod(token);
        getline(ss, token, ',');
        t.velocity_score = token.empty() ? 0.0 : stod(token);
        getline(ss, token, ',');
        t.geo_anomaly_score = token.empty() ? 0.0 : stod(token);
        getline(ss, t.payement_channel, ',');
        getline(ss, t.ip_address, ',');
        getline(ss, t.device_hash, ',');
        
        if (t.payement_channel == "card") insert(channels.card, t);
        else if (t.payement_channel == "ACH") insert(channels.ach, t);
        else if (t.payement_channel == "wire_transfer") insert(channels.wire_transfer, t);
        else if (t.payement_channel == "UPI") insert(channels.upi, t);
      }
      file.close();
    }
    
    // Calculate memory usage for a linked list
    size_t calculateMemoryUsage(Node *head)
    {
      size_t memory = 0;
      Node *current = head;
      while (current != nullptr)
      {
        memory += sizeof(Node) + sizeof(Transaction);
        current = current->next;
      }
      return memory;
    }
    
    // Export search results to JSON
    void exportSearchResultsToJson(Node *head, const string &type, long long timeMs, double memoryMB, size_t spaceUsed)
    {
      json j;
      j["operation"] = "search";
      j["data_structure"] = "linked_list";
      j["search_type"] = type;
      j["execution_time_ms"] = timeMs;
      j["memory_usage_mb"] = memoryMB;
      j["space_used_bytes"] = spaceUsed;
      
      json transactions = json::array();
      Node *current = head;
      while (current != nullptr)
      {
        if (current->data.transaction_type == type)
        {
          json transaction;
          transaction["id"] = current->data.id;
          transaction["amount"] = current->data.amount;
          transaction["location"] = current->data.location;
          transaction["is_fraud"] = current->data.is_fraud;
          transactions.push_back(transaction);
        }
        current = current->next;
      }
      j["matches"] = transactions;
      ofstream file("search_results_list.json");
      file << setw(4) << j << endl;
    }
    
    // Export sort results to JSON
    void exportSortResultsToJson(Node *head, long long timeMs, double memoryMB, size_t spaceUsed)
    {
      json j;
      j["operation"] = "location_sort";
      j["data_structure"] = "linked_list";
      j["execution_time_ms"] = timeMs;
      j["memory_usage_mb"] = memoryMB;
      j["space_used_bytes"] = spaceUsed;
      
      json transactions = json::array();
      Node *current = head;
      while (current != nullptr)
      {
        json transaction;
        transaction["id"] = current->data.id;
        transaction["location"] = current->data.location;
        transactions.push_back(transaction);
        current = current->next;
      }
      j["sorted_transactions"] = transactions;
      ofstream file("sort_results_list.json");
      file << setw(4) << j << endl;
    }
    
    // Deallocate all nodes in the lists
    void cleanup(ChannelLists &channels) {
      auto deleteList = [](Node*& head) {
        Node* current = head;
        while (current != nullptr) {
          Node* next = current->next;
          delete current;
          current = next;
        }
        head = nullptr;
      };
      deleteList(channels.card);
      deleteList(channels.ach);
      deleteList(channels.wire_transfer);
      deleteList(channels.upi);
    }
  }


//==================================================================================
  // ARRAY IMPLEMENTATION
//==================================================================================
  
  namespace DynamicArray {
    // Structure to manage a dynamic array of Transactions
    struct TransactionArray
    {
      Transaction *data;
      int size;
      int capacity;
      TransactionArray() : data(nullptr), size(0), capacity(0) {}
    };
    
    struct ChannelArrays
    {
      TransactionArray card;
      TransactionArray ach;
      TransactionArray wire_transfer;
      TransactionArray upi;
    };
    
    // Insert a transaction into a dynamic array, resizing if necessary
    void insert(TransactionArray &arr, const Transaction &t)
    {
      if (arr.size >= arr.capacity)
      {
        int new_capacity = (arr.capacity == 0) ? 10 : arr.capacity * 2;
        Transaction *new_data = new Transaction[new_capacity];
        for (int i = 0; i < arr.size; ++i)
        {
          new_data[i] = arr.data[i];
        }
        delete[] arr.data;
        arr.data = new_data;
        arr.capacity = new_capacity;
      }
      arr.data[arr.size++] = t;
    }
    
    // Search by transaction type (linear search)
    void searchByTransactionType(const TransactionArray &arr, const string &type)
    {
      int matchCount = 0;
      for (int i = 0; i < arr.size; ++i)
      {
        if (arr.data[i].transaction_type == type)
        {
          matchCount++;
        }
      }
      if (matchCount == 0)
        cout << "No transactions found with type: " << type << endl;
      else
        cout << matchCount << " transactions found with type: " << type << endl;
    }
    
    // Helper to merge two sorted subarrays
    void merge(Transaction *array, int const left, int const mid, int const right)
    {
      int const subArrayOne = mid - left + 1;
      int const subArrayTwo = right - mid;
      Transaction *leftArray = new Transaction[subArrayOne];
      Transaction *rightArray = new Transaction[subArrayTwo];
      for (int i = 0; i < subArrayOne; i++) leftArray[i] = array[left + i];
      for (int j = 0; j < subArrayTwo; j++) rightArray[j] = array[mid + 1 + j];
      auto i = 0, j = 0;
      int k = left;
      while (i < subArrayOne && j < subArrayTwo)
      {
        if (leftArray[i].location <= rightArray[j].location) array[k++] = leftArray[i++];
        else array[k++] = rightArray[j++];
      }
      while (i < subArrayOne) array[k++] = leftArray[i++];
      while (j < subArrayTwo) array[k++] = rightArray[j++];
      delete[] leftArray;
      delete[] rightArray;
    }
    
    // Merge sort main function for an array
    void mergeSortByLocation(Transaction *array, int const begin, int const end)
    {
      if (begin >= end) return;
      int mid = begin + (end - begin) / 2;
      mergeSortByLocation(array, begin, mid);
      mergeSortByLocation(array, mid + 1, end);
      merge(array, begin, mid, end);
    }
    
    // Read CSV and populate the arrays
    void readCSV(const string &filename, ChannelArrays &channels)
    {
      ifstream file(filename);
      if (!file.is_open()) {
        cout << "Error opening file!" << endl;
        return;
      }
      string line;
      getline(file, line); // Skip header
      while (getline(file, line))
      {
        stringstream ss(line);
        string token;
        Transaction t;
        getline(ss, t.id, ',');
        getline(ss, t.timestamp, ',');
        getline(ss, t.sender_account, ',');
        getline(ss, t.reciver_amount, ',');
        getline(ss, token, ',');
        t.amount = token.empty() ? 0.0f : stof(token);
        getline(ss, t.transaction_type, ',');
        getline(ss, t.merchant_category, ',');
        getline(ss, t.location, ',');
        getline(ss, t.device_used, ',');
        getline(ss, token, ',');
        t.is_fraud = (token == "1" || token == "true");
        getline(ss, t.fraud_type, ',');
        getline(ss, token, ',');
        t.time_since_last_transaction = token.empty() ? 0.0 : stod(token);
        getline(ss, token, ',');
        t.spending_deviation_score = token.empty() ? 0.0 : stod(token);
        getline(ss, token, ',');
        t.velocity_score = token.empty() ? 0.0 : stod(token);
        getline(ss, token, ',');
        t.geo_anomaly_score = token.empty() ? 0.0 : stod(token);
        getline(ss, t.payement_channel, ',');
        getline(ss, t.ip_address, ',');
        getline(ss, t.device_hash, ',');
        
        if (t.payement_channel == "card") insert(channels.card, t);
        else if (t.payement_channel == "ACH") insert(channels.ach, t);
        else if (t.payement_channel == "wire_transfer") insert(channels.wire_transfer, t);
        else if (t.payement_channel == "UPI") insert(channels.upi, t);
      }
      file.close();
    }
    
    // Calculate memory usage for an array
    size_t calculateMemoryUsage(const TransactionArray &arr)
    {
      // UPDATED: Calculate based on the actual number of elements (size)
      // rather than the total allocated memory (capacity). This gives a more
      // intuitive measure of the memory being used by the data itself.
      return static_cast<size_t>(arr.size) * sizeof(Transaction);
    }
    
    // Export search results to JSON
    void exportSearchResultsToJson(const TransactionArray &arr, const string &type, long long timeMs, double memoryMB, size_t spaceUsed)
    {
      json j;
      j["operation"] = "search";
      j["data_structure"] = "array";
      j["search_type"] = type;
      j["execution_time_ms"] = timeMs;
      j["memory_usage_mb"] = memoryMB;
      j["space_used_bytes"] = spaceUsed;
      
      json transactions = json::array();
      for (int i = 0; i < arr.size; ++i)
      {
        if (arr.data[i].transaction_type == type)
        {
          json transaction;
          transaction["id"] = arr.data[i].id;
          transaction["amount"] = arr.data[i].amount;
          transaction["location"] = arr.data[i].location;
          transaction["is_fraud"] = arr.data[i].is_fraud;
          transactions.push_back(transaction);
        }
      }
      j["matches"] = transactions;
      ofstream file("search_results_array.json");
      file << setw(4) << j << endl;
    }
    
    // Export sort results to JSON
    void exportSortResultsToJson(const TransactionArray &arr, long long timeMs, double memoryMB, size_t spaceUsed)
    {
      json j;
      j["operation"] = "location_sort";
      j["data_structure"] = "array";
      j["execution_time_ms"] = timeMs;
      j["memory_usage_mb"] = memoryMB;
      j["space_used_bytes"] = spaceUsed;
      
      json transactions = json::array();
      for (int i = 0; i < arr.size; ++i)
      {
        json transaction;
        transaction["id"] = arr.data[i].id;
        transaction["location"] = arr.data[i].location;
        transactions.push_back(transaction);
      }
      j["sorted_transactions"] = transactions;
      ofstream file("sort_results_array.json");
      file << setw(4) << j << endl;
    }
    
    // Deallocate all dynamic arrays
    void cleanup(ChannelArrays &channels)
    {
      delete[] channels.card.data;
      delete[] channels.ach.data;
      delete[] channels.wire_transfer.data;
      delete[] channels.upi.data;
    }
  }

// --- SHARED UTILITY FUNCTIONS ---
  void showPerformanceMetrics(const string &operation, long long timeMs, double memoryMB, size_t spaceUsed)
{
  cout << "\n+------------------------------- PERFORMANCE METRICS -------------------------------+" << endl;
  cout << "| Operation         | " << setw(60) << left << operation << "|\n";
  cout << "|-------------------+--------------------------------------------------------------|" << endl;
  cout << "| Execution Time    | " << setw(60) << left << (to_string(timeMs) + " ms") << "|\n";
  cout << "| Memory Usage      | " << setw(60) << left << (to_string(memoryMB) + " MB") << "|\n";
  cout << "| Space Used        | " << setw(60) << left << (to_string(spaceUsed) + " bytes") << "|\n";
  cout << "+----------------------------------------------------------------------------------+" << endl;
  cout << "Press Enter to continue...";
  cin.ignore(); // Clears the buffer
  cin.get();    // Waits for Enter key
  }


// --- MAIN PROGRAM LOGIC ---
  void runLinkedListImplementation() {
    LinkedList::ChannelLists channels;
    cout << "Loading data into Linked Lists..." << endl;
    LinkedList::readCSV("financial_fraud_detection_dataset.csv", channels);
    
    while (true) {
      cout << "\n--- Linked List Menu ---" << endl;
      cout << "1. Search by Transaction Type" << endl;
      cout << "2. Sort Transactions by Location" << endl;
      cout << "0. Back to Main Menu" << endl;
      cout << "Enter your choice: ";
      int choice;
      cin >> choice;
      cin.ignore();
      
      if (choice == 0) break;
      
      cout << "\nSelect Payment Channel:\n1. Card\n2. ACH\n3. Wire Transfer\n4. UPI\nChoice: ";
      int channelChoice;
      cin >> channelChoice;
      cin.ignore();
      
      LinkedList::Node* selectedChannel = nullptr;
      switch (channelChoice) {
        case 1: selectedChannel = channels.card; break;
        case 2: selectedChannel = channels.ach; break;
        case 3: selectedChannel = channels.wire_transfer; break;
        case 4: selectedChannel = channels.upi; break;
        default: cout << "Invalid channel!" << endl; continue;
      }
      
      if (!selectedChannel) {
        cout << "Channel is empty or invalid!" << endl;
        continue;
      }
      
      if (choice == 1) {
        cout << "Enter transaction type to search: ";
        string type;
        getline(cin, type);
        auto start = chrono::high_resolution_clock::now();
        LinkedList::searchByTransactionType(selectedChannel, type);
        auto end = chrono::high_resolution_clock::now();
        auto duration = chrono::duration_cast<chrono::milliseconds>(end - start);
        size_t mem = LinkedList::calculateMemoryUsage(selectedChannel);
        double memMB = static_cast<double>(mem) / (1024 * 1024);
        LinkedList::exportSearchResultsToJson(selectedChannel, type, duration.count(), memMB, mem);
        showPerformanceMetrics("Search (Linked List)", duration.count(), memMB, mem);
      } else if (choice == 2) {
        auto start = chrono::high_resolution_clock::now();
        if (channelChoice == 1) channels.card = LinkedList::mergeSortByLocation(selectedChannel);
        else if (channelChoice == 2) channels.ach = LinkedList::mergeSortByLocation(selectedChannel);
        else if (channelChoice == 3) channels.wire_transfer = LinkedList::mergeSortByLocation(selectedChannel);
        else if (channelChoice == 4) channels.upi = LinkedList::mergeSortByLocation(selectedChannel);
        auto end = chrono::high_resolution_clock::now();
        auto duration = chrono::duration_cast<chrono::milliseconds>(end - start);
        size_t mem = LinkedList::calculateMemoryUsage(selectedChannel);
        double memMB = static_cast<double>(mem) / (1024 * 1024);
        LinkedList::exportSortResultsToJson(selectedChannel, duration.count(), memMB, mem);
        showPerformanceMetrics("Sort (Linked List)", duration.count(), memMB, mem);
      }
    }
    LinkedList::cleanup(channels);
  }

void runArrayImplementation() {
  DynamicArray::ChannelArrays channels;
  cout << "Loading data into Dynamic Arrays..." << endl;
  DynamicArray::readCSV("financial_fraud_detection_dataset.csv", channels);
  
  while (true) {
    cout << "\n--- Array Menu ---" << endl;
    cout << "1. Search by Transaction Type" << endl;
    cout << "2. Sort Transactions by Location" << endl;
    cout << "0. Back to Main Menu" << endl;
    cout << "Enter your choice: ";
    int choice;
    cin >> choice;
    cin.ignore();
    
    if (choice == 0) break;
    
    cout << "\nSelect Payment Channel:\n1. Card\n2. ACH\n3. Wire Transfer\n4. UPI\nChoice: ";
    int channelChoice;
    cin >> channelChoice;
    cin.ignore();
    
    DynamicArray::TransactionArray* selectedChannel = nullptr;
    switch(channelChoice) {
      case 1: selectedChannel = &channels.card; break;
      case 2: selectedChannel = &channels.ach; break;
      case 3: selectedChannel = &channels.wire_transfer; break;
      case 4: selectedChannel = &channels.upi; break;
      default: cout << "Invalid channel!" << endl; continue;
    }
    
    if (!selectedChannel || selectedChannel->size == 0) {
      cout << "Channel is empty or invalid!" << endl;
      continue;
    }
    
    if (choice == 1) {
      cout << "Enter transaction type to search: ";
      string type;
      getline(cin, type);
      auto start = chrono::high_resolution_clock::now();
      DynamicArray::searchByTransactionType(*selectedChannel, type);
      auto end = chrono::high_resolution_clock::now();
      auto duration = chrono::duration_cast<chrono::milliseconds>(end - start);
      size_t mem = DynamicArray::calculateMemoryUsage(*selectedChannel);
      double memMB = static_cast<double>(mem) / (1024 * 1024);
      DynamicArray::exportSearchResultsToJson(*selectedChannel, type, duration.count(), memMB, mem);
      showPerformanceMetrics("Search (Array)", duration.count(), memMB, mem);
    } else if (choice == 2) {
      auto start = chrono::high_resolution_clock::now();
      DynamicArray::mergeSortByLocation(selectedChannel->data, 0, selectedChannel->size - 1);
      auto end = chrono::high_resolution_clock::now();
      auto duration = chrono::duration_cast<chrono::milliseconds>(end - start);
      size_t mem = DynamicArray::calculateMemoryUsage(*selectedChannel);
      double memMB = static_cast<double>(mem) / (1024 * 1024);
      DynamicArray::exportSortResultsToJson(*selectedChannel, duration.count(), memMB, mem);
      showPerformanceMetrics("Sort (Array)", duration.count(), memMB, mem);
    }
  }
  DynamicArray::cleanup(channels);
}


int main()
{
  while (true)
  {
    cout << "\n+-------------------- MAIN MENU ----------------+" << endl;
    cout << "| Select the data structure to use:               |" << endl;
    cout << "| 1. Linked List Implementation                   |" << endl;
    cout << "| 2. Array Implementation                         |" << endl;
    cout << "| 0. Exit Program                                 |" << endl;
    cout << "+-------------------------------------------------+" << endl;
    cout << "Enter your choice: ";
    
    int choice;
    cin >> choice;
    cin.ignore(); // Consume the newline character
    
    switch (choice)
    {
      case 1:
        runLinkedListImplementation();
      break;
      case 2:
        runArrayImplementation();
      break;
      case 0:
        cout << "Exiting program. Goodbye!" << endl;
      return 0;
      default:
        cout << "Invalid choice. Please try again." << endl;
      break;
    }
  }
  
  return 0;
}