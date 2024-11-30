case class BoundTree(root: Symbol,
                     diagnostics: Diagnostics,
                     functions: Array[BoundFunction],
                     fields: Array[BoundField])
