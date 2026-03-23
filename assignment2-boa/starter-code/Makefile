# Makefile for Boa Compiler
# Replace 'elf64' with 'macho64' on macOS

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
test: test/simple.run test/add.run test/binop.run test/let_simple.run test/let_multi.run test/nested.run
	@echo "Running tests..."
	@echo -n "simple (expected 42): " && ./test/simple.run
	@echo -n "add (expected 6): " && ./test/add.run
	@echo -n "binop (expected 9): " && ./test/binop.run
	@echo -n "let_simple (expected 10): " && ./test/let_simple.run
	@echo -n "let_multi (expected 11): " && ./test/let_multi.run
	@echo -n "nested (expected 25): " && ./test/nested.run

.PHONY: clean test
