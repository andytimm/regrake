# regrake 1.0 Release Todo List

## Core Functionality (High Priority)
- [ ] Implement KL Loss for divergence-based losses
  - Important for certain types of statistical matching
  - Already implemented in Python version
  - Need to handle edge cases (zeros, infinities)

- [ ] Add support for continuous variables
  - Currently only handles categorical variables effectively
  - Note that this isn't binning, it's handling actual continuous inputs
  - Add examples in documentation

- [ ] Improve solver robustness
  - Better handling of NA values in input data
  - Improved numerical stability checks

## Package Documentation
- [ ] Complete roxygen documentation for all exported functions
  - Ensure all parameters are documented
  - Add meaningful examples
  - Include edge cases and warnings

- [ ] Create basic vignette covering:
  - Simple reg raking example
  - Handling different population data formats
  - Using different regularization types
  - Working with continuous variables
  - Common pitfalls and solutions

- [ ] Set up pkgdown website
  - Basic theme and structure
  - Landing page with quick start
  - Function reference
  - Vignette access
  - Link to GitHub

## CRAN Preparation
- [ ] Update DESCRIPTION file
  - Complete all fields
  - Check dependencies
  - Add proper version numbers
  - Include proper author/maintainer info

- [ ] Ensure R CMD check passes with no warnings
  - Run checks on multiple R versions
  - Test on different OS
  - Address any NOTEs

- [ ] Complete package documentation
  - man/ pages for all exports
  - Dataset documentation if needed
  - NEWS.md for version history

## Testing
- [ ] Add comprehensive tests for:
  - KL Loss functionality
  - Continuous variable handling
  - Edge cases in population data
  - Different regularization types
  - Error conditions and recovery
  - Solver convergence and stability
  - Memory usage with large datasets

## Error Handling
- [ ] Improve input validation
  - Better error messages for common mistakes
  - Validation for control parameters
  - Checks for data consistency
  - Warning for potential convergence issues

## Known Issues/Limitations to Document
1. Current limitations with continuous variables
2. Performance characteristics for large datasets
3. Currently only taking 1 target format
4. NA handling behavior

## Future Enhancements (Post 1.0)
- Inequality constraints
- More target input formats

## Implementation Notes
- Python reference implementation in `ai_context/rswjax/`
- Key differences documented in tests
- Performance benchmarks available in `tests/`