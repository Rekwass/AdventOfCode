module Lib.Algorithm (dijkstra, dijkstraAllShortestPaths, bronKerboschPivot) where

import Data.Hashable (Hashable)
import qualified Data.HashPSQ as H
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe (fromJust, mapMaybe)
import Data.List (mapAccumL)

dijkstra :: (Num c, Ord c, Hashable v, Ord v) => H.HashPSQ v c c -> M.Map v c -> M.Map v v -> (v -> [(v, c)]) -> (v -> Bool) -> Maybe (M.Map v v)
dijkstra queue dist prev getNeighbours isGoal
    | H.null queue = Nothing
    | isGoal current = Just prev
    | otherwise = dijkstra queue' dist' prev' getNeighbours isGoal
    where
      (current, _, distStart, rest) = fromJust . H.minView $ queue
      ngbrs = mapMaybe consider . getNeighbours $ current
      queue' = foldr (\(nbr, cost) -> H.insert nbr cost cost) rest ngbrs
      dist' = foldr (uncurry M.insert) dist ngbrs
      prev' = foldr (\(ngbr, _) -> M.insert ngbr current) prev ngbrs
      consider (ngb, dstNgbr)
        | ngb `M.notMember` dist || newDist < dist M.! ngb = Just (ngb, newDist)
        | otherwise = Nothing
        where
          newDist = distStart + dstNgbr

dijkstraAllShortestPaths :: (Num c, Ord c, Hashable v, Ord v) => H.HashPSQ v c c -> M.Map v c -> M.Map v (S.Set v) -> (v -> [(v, c)]) -> (v -> Bool) -> M.Map v (S.Set v)
dijkstraAllShortestPaths queue dist prev getNeighbours isGoal
    | isGoal current = paths
    | otherwise = dijkstraAllShortestPaths queue' dist' prev' getNeighbours isGoal
    where
      (current, _, distStart, rest) = fromJust . H.minView $ queue
      ngbrs = mapMaybe consider . getNeighbours $ current
      queue' = foldr (\(nbr, cost) -> H.insert nbr cost cost) rest ngbrs
      dist' = foldr (uncurry M.insert) dist ngbrs
      prev' = foldr (M.alter (update current) . fst) prev ngbrs
      update pCurr Nothing = Just . S.singleton $ pCurr
      update pCurr (Just pNgb) = Just . S.insert pCurr $ pNgb
      consider (ngb, dstNgbr)
        | ngb `M.notMember` dist || newDist <= dist M.! ngb = Just (ngb, newDist)
        | otherwise = Nothing
        where
          newDist = distStart + dstNgbr
      paths = foldr M.delete prev . filter ((> bestDist) . (dist M.!)) $ goalNodes
      goalNodes = filter isGoal . M.keys $ dist
      bestDist = minimum . map (dist M.!) $ goalNodes


{-
Pseudocode from: https://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm#With_pivoting

R = clique
P = candidates
X = excluded

algorithm BronKerbosch2(R, P, X) is
    if P and X are both empty then
        report R as a maximal clique
    choose a pivot vertex u in P ⋃ X
    for each vertex v in P \ N(u) do
        BronKerbosch2(R ⋃ {v}, P ⋂ N(v), X ⋂ N(v))
        P := P \ {v}
        X := X ⋃ {v}
-}
-- | 'bronKerboschPivot' @pickPivot getNeighbours vertices@ Retrieve the maximal cliques in a Set
bronKerboschPivot :: Ord a => (S.Set a -> S.Set a -> a) -> (a -> S.Set a) -> S.Set a -> [S.Set a]
bronKerboschPivot pickPivot getNeighbours vertices = go S.empty vertices S.empty
  where
    go r p x
      | S.null p && S.null x = [r]
      | otherwise = concat . snd . mapAccumL step (p, x) . S.elems $ nPivot
        where
          nPivot = p S.\\ getNeighbours pivot
          pivot = pickPivot p x
          step (p', x') v = ((S.delete v p', S.insert v x'), go (S.insert v r) (S.intersection p' nv) (S.intersection x' nv))
            where
              nv = getNeighbours v

{-
-- | networkx enumerate all cliques implementation

@not_implemented_for("directed")
@nx._dispatchable
def enumerate_all_cliques(G):
    """Returns all cliques in an undirected graph.

    This function returns an iterator over cliques, each of which is a
    list of nodes. The iteration is ordered by cardinality of the
    cliques: first all cliques of size one, then all cliques of size
    two, etc.

    Parameters
    ----------
    G : NetworkX graph
        An undirected graph.

    Returns
    -------
    iterator
        An iterator over cliques, each of which is a list of nodes in
        `G`. The cliques are ordered according to size.

    Notes
    -----
    To obtain a list of all cliques, use
    `list(enumerate_all_cliques(G))`. However, be aware that in the
    worst-case, the length of this list can be exponential in the number
    of nodes in the graph (for example, when the graph is the complete
    graph). This function avoids storing all cliques in memory by only
    keeping current candidate node lists in memory during its search.

    The implementation is adapted from the algorithm by Zhang, et
    al. (2005) [1]_ to output all cliques discovered.

    This algorithm ignores self-loops and parallel edges, since cliques
    are not conventionally defined with such edges.

    References
    ----------
    .. [1] Yun Zhang, Abu-Khzam, F.N., Baldwin, N.E., Chesler, E.J.,
           Langston, M.A., Samatova, N.F.,
           "Genome-Scale Computational Approaches to Memory-Intensive
           Applications in Systems Biology".
           *Supercomputing*, 2005. Proceedings of the ACM/IEEE SC 2005
           Conference, pp. 12, 12--18 Nov. 2005.
           <https://doi.org/10.1109/SC.2005.29>.

    """
    index = {}
    nbrs = {}
    for u in G:
        index[u] = len(index)
        # Neighbors of u that appear after u in the iteration order of G.
        nbrs[u] = {v for v in G[u] if v not in index}

    queue = deque(([u], sorted(nbrs[u], key=index.__getitem__)) for u in G)
    # Loop invariants:
    # 1. len(base) is nondecreasing.
    # 2. (base + cnbrs) is sorted with respect to the iteration order of G.
    # 3. cnbrs is a set of common neighbors of nodes in base.
    while queue:
        base, cnbrs = map(list, queue.popleft())
        yield base
        for i, u in enumerate(cnbrs):
            # Use generators to reduce memory consumption.
            queue.append(
                (
                    chain(base, [u]),
                    filter(nbrs[u].__contains__, islice(cnbrs, i + 1, None)),
                )
            )
-}
