# Makefile for Adder Compiler
# Replace 'elf64' with 'macho64' on macOS

SNEK_TESTS := $(filter-out test/example.snek test/add_overflow_error.snek,$(wildcard test/*.snek))
RUN_TESTS := $(patsubst %.snek,%.run,$(SNEK_TESTS))

# Pattern rule to compile .snek files to .s assembly files
test/%.s: test/%.snek src/main.rs
	cargo run -- $< test/$*.s

# Pattern rule to assemble .s files and link into executables
# Change -f elf64 to -f macho64 on macOS
test/%.run: test/%.s runtime/start.rs
	nasm -f elf64 test/$*.s -o runtime/our_code.o
	ar rcs runtime/libour_code.a runtime/our_code.o
	rustc -L runtime/ runtime/start.rs -o test/$*.run

# Clean build artifacts
clean:
	cargo clean
	rm -f test/*.s test/*.run runtime/*.o runtime/*.a

# Run all tests
test: $(RUN_TESTS)
	@echo "Built test executables:"
	@for run_file in $(RUN_TESTS); do echo "$$run_file"; done

.PHONY: clean test
