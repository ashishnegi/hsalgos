#include <map>
#include <vector>
#include <fstream>
#include <iostream>
#include <set>
#include <numeric>
#include <algorithm>

// Solution for : skiing in singapore
// All areas are nodes. There is a directed edge between two adjacent nodes (node1 -> node2)
// if node1 has higher elevation than node2.
// Problem is converted into a DAG and Dfs is run on it from all nodes which do not have any incoming edges
// and maximum depth covered with highest drop value is taken as result.

// compile and run :
// g++ --std=c++14 cpp/LongestEdgePathDag.cpp -o longest
// time ./longest map.txt

/*
  Node stores its height and list of outgoing-edges from itself.
 */
struct Node
{
  int height;
  std::vector<int> outGoingEdges;
  Node(int h, const std::vector<int> & o) : height(h), outGoingEdges(o) {}
  Node() : height(0), outGoingEdges(std::vector<int>()) {}
};

/*
  DfsResult : Result of a dfs call on a node;
 */
struct DfsResult
{
  int depth; // depth uptill which dfs went.
  int drop; // highest drop-value that it found.
  std::vector<int> path; // path that dfs took
  DfsResult(int d, int dr) : depth(d), drop(dr) {}

  // DfsResult maximizes depth and drop
  bool operator<(const DfsResult & d2) const {
    if (depth == d2.depth) {
      return drop < d2.drop;
    }
    return depth < d2.depth;
  }
};

// for some debugging
template <class T> void print(const T & sourceNodes);

// api to do the depth-first-search
DfsResult dfs(int node, const std::vector<Node> & graph);

// test if answer has a valid path or not
bool isValidPath(const std::vector<int> & path, const std::vector<Node> & graph, int width);

// Generates the graph, set of nodes with incoming edges and list of all ids.
void generateGraphData(const std::vector< std::vector<int> > & rawData, const int width, const int height,
		       std::vector<Node> & graph, std::set<int> & nodesWithInEdges, std::vector<int> & allIds);

int main(int argc, char *argv[]) {
  if (argc < 2) {
    std::cout << "Please give a filename.";
    return -1;
  }

  using namespace std;

  cout << "Reading file: " << argv[1] << endl;

  fstream file;
  file.open(argv[1], fstream::in);

  int width = 0, height = 0;
  file >> width >> height;

  vector< vector<int> >  data;

  // data has one layer at top/bottom/left/right to cover actual data.
  // so actual indexing is 1-based.
  data.push_back(vector<int>(width, 1501));
  for (int h=0; h < height; ++h) {
    int height = 0;
    vector<int> heights(1,1501);
    for (int w=0; w < width; ++w) {
      file >> height;
      heights.push_back(height);
    }
    heights.push_back(1501);
    data.push_back(heights);
  }
  data.push_back(vector<int>(width, 1501));

  // graph has width * height num-nodes :- each having list of nodes
  // which have lower height that itself.
  // id of each node is its-row * width + its-col
  int newWidth = width + 2, newHeight = height + 2;
  vector<Node> graph(newWidth * newHeight);
  set<int> nodesWithInEdges;
  vector<int> allIds;

  // Generate the graph
  generateGraphData(data, width, height, graph, nodesWithInEdges, allIds);

  // node ids range
  vector<int> sourceNodes(width * height);
  vector<int>::iterator it;
  it = set_difference(allIds.begin(), allIds.end(), nodesWithInEdges.begin(), nodesWithInEdges.end(), sourceNodes.begin());
  sourceNodes.resize(it - sourceNodes.begin());

  // print(allIds);
  // print(nodesWithInEdges);
  // print(sourceNodes);

  // clear all the data that we do not need now.. reduce memory consumption.
  data.clear();
  allIds.clear();
  nodesWithInEdges.clear();

  // we have now : sourceNodes; graph : has height
  // run the dfs on Dag
  DfsResult maxDepthAndDrop(0,0);
  for (auto sourceNode : sourceNodes) {
    auto dfsData = dfs(sourceNode, graph);
    // drop is taken from source height to lowest drop to which i could go in dfs.
    dfsData.drop = graph[sourceNode].height - dfsData.drop;
    if (maxDepthAndDrop < dfsData) {
      maxDepthAndDrop = dfsData;
    }
  }

  // print the results.
  cout << maxDepthAndDrop.depth << " " << maxDepthAndDrop.drop << " path: ";
  // print(maxDepthAndDrop.path);
  for (auto p : maxDepthAndDrop.path) {
    cout << graph[p].height << " ";
  }
  cout << "\nValid path: " << (isValidPath(maxDepthAndDrop.path, graph, newWidth) ? "true"  : "false");
  return 0;
}

// Drop is overloaded term : while in dfs it means lowest drop-height
// and after dfs result it means vertical-drop that we took.
bool compareDepthAndDrop(const DfsResult & d1, const DfsResult & d2) {
  // maximize depth and take lowest height that we could reach.
  if (d1.depth == d2.depth) {
    return d1.drop > d2.drop;
  }
  return d1.depth < d2.depth;
}

DfsResult dfs(int node, const std::vector<Node> & graph) {
  using namespace std;
  auto edges = graph[node].outGoingEdges;

  // we store lowest height that we could reach from this node.
  DfsResult maxDepthAndDrop(0, graph[node].height);
  for (auto edge : edges) {
    DfsResult edgeData = dfs(edge, graph);
    if (compareDepthAndDrop(maxDepthAndDrop, edgeData)) {
      maxDepthAndDrop = edgeData;
    }
  }

  maxDepthAndDrop.depth += 1; // for myself.
  maxDepthAndDrop.path.push_back(node);
  return maxDepthAndDrop;
}

bool isValidPath(const std::vector<int> & path, const std::vector<Node> & graph, int width) {
  for (int i = 1; i < path.size(); ++i) {
    int diffPath = path[i] - path[i-1];
    if (!((diffPath == width) || // down.
	  (diffPath == -width) || // up
	  ((diffPath == 1) && (path[i] % width != 0)) || // left
	  ((diffPath == -1) && (path[i] % width != (width - 1))))  // right
	|| (graph[path[i]].height <= graph[path[i-1]].height)) // height should increase always.
      {
	std::cout << std::endl << "Bad path: " << path[i-1] << " " << path[i] ;
	return false;
      }
  }

  return true;
}

template <class T> void print(const T & sourceNodes)
{
  std::cout << std::endl;
  for (const auto & i: sourceNodes)
    std::cout << i << ' ';
  std::cout << std::endl;
}

void generateGraphData(const std::vector< std::vector<int> > & data,
		       const int width, const int height,
		       std::vector<Node> & graph,
		       std::set<int> & nodesWithInEdges,
		       std::vector<int> & allIds) {
  int newWidth = width + 2, newHeight = height + 2;

  for (int r=1; r <= height; ++r) {
    for (int c=1; c <= width; ++c) {
      int upHeight = data[r-1][c]
	, downHeight = data[r+1][c]
	, leftHeight = data[r][c-1]
	, rightHeight = data[r][c+1]
	, myHeight = data[r][c]
	, myId = r * newWidth + c;

      allIds.push_back(myId);
      graph[myId].height = myHeight;

      if (upHeight < myHeight) {
	int upId = myId - newWidth;
	graph[myId].outGoingEdges.push_back(upId);
	nodesWithInEdges.insert(upId);
      }
      if (downHeight < myHeight) {
	int downId = myId + newWidth;
	graph[myId].outGoingEdges.push_back(downId);
	nodesWithInEdges.insert(downId);
      }
      if (leftHeight < myHeight) {
	graph[myId].outGoingEdges.push_back(myId - 1); // left id
	nodesWithInEdges.insert(myId - 1);
      }
      if (rightHeight < myHeight) {
	graph[myId].outGoingEdges.push_back(myId + 1); // right id
	nodesWithInEdges.insert(myId + 1);
      }
    }
  }
}
