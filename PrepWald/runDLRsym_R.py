import sympy

def create_symbolic_matrices(nv, ng):
    # Create symbolic matrices A, D, and C
    A = sympy.Matrix(nv, nv, lambda row, col: sympy.Symbol(f'a{row+1}{col+1}'))
    D = sympy.Matrix(nv, ng-1, lambda row, col: sympy.Symbol(f'd{row+1}{col+1}'))
    

    # Compute the inner matrix
    identity_matrix = sympy.eye(nv)
    inner = identity_matrix - A

    # Compute the inverse of C
    inverseInner = inner.inv()

    # Calculate DLRsym
    DLRsym = inverseInner * D

    # Return matrices and results
    return {
        'A': A,
        'D': D,
        'inner': inner,
        'inverseInner': inverseInner,
        'DLRsym': DLRsym
    }

# Example usage
if __name__ == "__main__":
    nv = 6  # Example size
    ng = 3  # Example size

    results = create_symbolic_matrices(nv, ng)

    # Print results
    print("Matrix A:")
    print(results['A'])
    print("\nMatrix D:")
    print(results['D'])
    print("\nMatrix Inner:")
    print(results['inner'])
    print("\nInverse of Inner:")
    print(results['inverseInner'])
    print("\nDLRsym:")
    print(results['DLRsym'])
