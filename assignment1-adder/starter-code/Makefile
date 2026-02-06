# Makefile for Adder Compiler
# Replace 'elf64' with 'macho64' on macOS

SNEK_FILES := $(wildcard test/*.snek)
ASM_FILES := $(patsubst test/%.snek,test/%.s,$(SNEK_FILES))
RUN_FILES := $(patsubst test/%.snek,test/%.run,$(SNEK_FILES))

.SECONDARY: $(ASM_FILES)

# Pattern rule to compile .snek files to .s assembly files
test/%.s: test/%.snek src/main.rs
	cargo run -- $< test/$*.s

# Pattern rule to assemble .s files and link into executables
# Change -f elf64 to -f macho64 on macOS
test/%.run: test/%.s runtime/start.rs
	nasm -f macho64 test/$*.s -o runtime/our_code.o
	ar rcs runtime/libour_code.a runtime/our_code.o
	rustc --target x86_64-apple-darwin -L runtime/ runtime/start.rs -o test/$*.run

# Clean build artifacts
clean:
	cargo clean
	rm -f test/*.s test/*.run runtime/*.o runtime/*.a

# Run all tests
test: $(RUN_FILES)
	@echo "Running tests..."
	@for run_file in $(RUN_FILES); do ./$$run_file; done

test-verbose:
	@for snek_file in $(SNEK_FILES); do \
		base=$${snek_file%.snek}; \
		echo "===== $$snek_file ====="; \
		cat $$snek_file; \
		$(MAKE) -B $$base.run; \
		echo "===== $$base.s ====="; \
		cat $$base.s; \
		echo "===== $$base.run ====="; \
		./$$base.run; \
		echo ""; \
	done

.PHONY: clean test test-verbose
