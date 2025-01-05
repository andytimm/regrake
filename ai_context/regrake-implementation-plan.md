# regrake: Implementation Plan

## Package Overview

A modern R implementation of regularized raking that prioritizes intuitive interfaces while maintaining flexibility and power. Built on the mathematical framework from the Barratt et al. (2021) paper while providing an R-idiomatic interface.

### Core Design Philosophy

1. **Intuitive Interface**
- Clear, discoverable function names
- Formula interface that feels natural to R users
- Sensible defaults with power-user options
- Error messages that guide correct usage

2. **Staged Implementation**
- Start with rock-solid core functionality
- Add complexity gradually
- Maintain interface elegance throughout
- Clear about current limitations

3. **Flexible but Opinionated**
- Support common use cases elegantly
- Allow power-user access to internals
- Guide users toward good practices
- Clear documentation of choices

## Core Interfaces

### Main Function
```r
# Main interface - piped workflow
df %>%
  rsw(exact(race + educ) + l2(race:educ:income)) %>%
  group_by(race) %>%
  summarize(mean_y = weighted.mean(y, .weights))

# With targets specified
df %>%
  rsw(exact(race + educ),
      population = pop_df) %>%  # or targets = targets_df
  summarize(...)

# Complex balance using helpers (Phase 2)
df %>%
  rsw(hbal(race + educ + age)) %>%
  summarize(...)
```

### Direct Interface (Power Users)
```r
# Direct solver access
solve_rsw(
  design_matrix = X,
  targets = targets,
  losses = list(
    equality_loss(1:10, targets = state_props),
    l2_loss(11:20, targets = demo_props)
  ),
  regularizer = entropy_regularizer(limit = 5)
)
```

### Target Specification
```r
# Population data
rsw(..., population = pop_df)
rsw(..., population = pop_df, pop_weight = "weight")

# Pre-computed targets
rsw(..., targets = tibble(
  variable = c("race", "race"),
  level = c("white", "black"),
  proportion = c(0.7, 0.3)
))

# List format
rsw(..., targets = list(
  race = c(white = 0.7, black = 0.3)
))
```

## Implementation Phases

### Phase 1: Core Functionality

Focus on building a minimal but elegant foundation:

1. **Solver Implementation**
- Port ADMM solver from Python
- Focus on Matrix package integration
- Clear R interface to solver components
- Comprehensive tests against Python implementation

2. **Basic Formula Interface**
```r
# Must feel natural even in Phase 1
rsw(exact(race + educ) + l2(race:educ))

# Core formula components
exact() # Equality constraints
l2()    # L2 loss
```

3. **Initial Target Processing**
- Support autumn-style targets first
- Basic but thorough validation
- Clear error messages
- Foundation for other formats

4. **Minimal Output Structure**
```r
# Default pipeline usage returns weighted data
df %>%
  rsw(formula) %>%
  summarize(...)

# Verbose mode returns full object
fit <- rsw(formula, data = df, verbose = TRUE)
print(fit)   # Basic convergence info
summary(fit) # More details
```

### Phase 2: Interface Polish

1. **Extended Formula Interface**
- Add thorough validation
- Support all interaction types
- Add `hbal()` wrapper
- Improve error messages

2. **Full Target Support**
- Population data handling
- Weighted population data
- Better interaction handling
- Missing data policies

3. **Rich Output Methods**
- Full S3 method implementation
- Diagnostic functions
- Pipeline integration helpers
- Visualization tools

### Phase 3: Advanced Features

1. **Additional Capabilities**
- More loss types
- Continuous variables
- Complex targets
- Performance optimization

2. **Integration Features**
- survey package integration
- Advanced workflow helpers
- Additional diagnostic tools

## Implementation Notes

### Core Components (`R/`)

1. **Formula Processing** (`formula.R`)
- Parse raking formulas
- Convert to design matrices
- Validate specifications
- Handle interactions

2. **Target Processing** (`targets.R`)
- Convert input formats
- Validate targets
- Match to formula specs
- Handle special cases

3. **Solver Core** (`solver.R`)
- ADMM implementation
- Matrix operations
- Convergence checking
- Status tracking

4. **Loss & Regularizer** (`losses.R`, `regularizers.R`)
- Core loss implementations
- Regularizer implementations
- Validation helpers
- Clear class structure

5. **Main Interface** (`rsw.R`)
- Main rsw function
- Interface coordination
- Output structuring
- Pipeline integration

### Testing Strategy

1. **Unit Tests**
- Port Python tests
- Formula parsing tests
- Target validation tests
- Output structure tests

2. **Integration Tests**
- End-to-end workflows
- Common usage patterns
- Error cases
- Performance checks

### Documentation

1. **Function Documentation**
- Clear examples
- Explicit assumptions
- Usage guidance
- Cross-references

2. **Vignettes**
- Getting started guide
- Common workflows
- Advanced usage
- Best practices

## Dependencies

### Core
- Matrix (sparse operations)
- rlang (formula tools)

### Suggested
- dplyr (examples)
- survey (integration)

## Key Implementation Principles

1. **Interface First**
- Design user experience before implementation
- Make compromises explicit
- Clear upgrade paths

2. **Validation Throughout**
- Check inputs early
- Informative error messages
- Guide toward correct usage

3. **Performance Aware**
- Profile early
- Memory efficient
- Sparse operations

4. **Clear Documentation**
- Explain statistical concepts
- Show common patterns
- Document limitations

This implementation plan emphasizes building a solid foundation while maintaining clear paths to advanced functionality. The focus is on making each phase useful and elegant, even if not yet complete.