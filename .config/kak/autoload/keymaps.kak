# Movement
map global normal    h     h
map global normal    H     H
map global normal <a-h> <a-h>
map global normal <a-H> <a-H>

map global normal    n     j
map global normal    N     J
map global normal <a-n> <a-j>
map global normal <a-N> <a-J>

map global normal    e     k
map global normal    E     K
map global normal <a-e> <a-k>
map global normal <a-E> <a-K>

map global normal    i     l
map global normal    I     L
map global normal <a-i> <a-l>
map global normal <a-I> <a-L>

# Insert
map global normal    s     i
map global normal    S     I
map global normal <a-s> <a-i>
map global normal <a-S> <a-I>

# Search
map global normal    l     n
map global normal    L     N
map global normal <a-l> <a-n>
map global normal <a-L> <a-N>

# Open newline
map global normal    k     o
map global normal    K     O
map global normal <a-k> <a-o>
map global normal <a-K> <a-O>

# Goto
map global goto      h     ''
map global goto      l     ''
map global goto      k     ''
map global goto      j     ''
map global goto      h     h -docstring "line begin"
map global goto      i     l -docstring "line right"
map global goto      e     k -docstring "buffer begin"
map global goto      n     j -docstring "buffer end"
map global goto      s     i -docstring "first non blank"

# View
map global view      h     ''
map global view      l     ''
map global view      k     ''
map global view      j     ''
map global view      h     h -docstring "scroll left"
map global view      i     l -docstring "scroll right"
map global view      e     k -docstring "scroll up"
map global view      n     j -docstring "scroll down"

map global normal k s
map global normal K S
map global normal <c-k> <a-s>

map global normal t e
map global normal T E
