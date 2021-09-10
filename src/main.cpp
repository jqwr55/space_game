#include <iostream>
#include <math.h> 
#include <immintrin.h>

#include <glew.h>
#include <glfw3.h>

#ifdef __GNUC__
    #include <sys/mman.h> 
    #define _FORCE_INLINE __attribute__((always_inline))
    #define _TRAP __builtin_trap()
    #define OS_MEM_RW_REQUEST(x) mmap(nullptr, x, PROT_WRITE | PROT_READ, MAP_PRIVATE | MAP_ANONYMOUS, 0,0)
    #define OS_MEM_RELEASE(base, size) munmap(base, size)
#else
    #ifdef _MSC_VER
        #include <memoryapi.h> 
        #define _FORCE_INLINE
        #define _TRAP __debug_break();
        #define OS_MEM_RW_REQUEST(x) VirtualAlloc(nullptr, x, MEM_RESERVE | MEM_COMMIT)
        #define OS_MEM_RELEASE(base, size) VirtualFree(base, 0, MEM_RELEASE)
    #endif
#endif

typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef u8 byte;

typedef float f32;
typedef double f64;

static_assert(sizeof(byte) == 1);
static_assert(sizeof(i8) == 1 && sizeof(u8) == 1);
static_assert(sizeof(i16) == 2 && sizeof(u16) == 2);
static_assert(sizeof(i32) == 4 && sizeof(u32) == 4);
static_assert(sizeof(i64) == 8 && sizeof(u64) == 8);

static_assert(sizeof(f32) == 4);
static_assert(sizeof(f64) == 8);


constexpr u64 KILO_BYTE = 1000;
constexpr u64 MEGA_BYTE = 1000 * KILO_BYTE;
constexpr u64 GIGA_BYTE = 1000 * MEGA_BYTE;
constexpr u64 TERA_BYTE = 1000 * GIGA_BYTE;

constexpr u32 MICRO_SEC = 1000;
constexpr u32 MILI_SEC = 1000 * MICRO_SEC;


#define DEBUG_BUILD 1

#if DEBUG_BUILD == 1

    #define LOG_ASSERT(x , y ) if( !(x) ) {std::cerr << #x << " " << y << " " << __FILE__ << " " << __LINE__ << std::endl; _TRAP; }
    #define ASSERT(x) if( !(x) ) {std::cerr << #x << " triggered builtin trap in: " << __FILE__ << " " << __LINE__ << std::endl; _TRAP; }
    #define LOG(x) x;
    #define GL_CALL(x) GLClearError(); x; ASSERT(GLLogCall(#x , __FILE__ , __LINE__))
    #define GLFW_CALL(x) GLFWclearError(); x; ASSERT(GLFWlogCall(#x , __FILE__ , __LINE__));

    #define ALLOCATE(x) my_malloc_debug(x)
    #define FREE(x) my_free_debug(x)

#else
    
    #define GL_CALL(x) x
    #define GLFW_CALL(x) x

    #define LOG_ASSERT(x , y ) x
    #define ASSERT(x) x
    #define LOG(x) x

    #define ALLOCATE(x) my_malloc(x)
    #define FREE(x) my_free(x)

#endif

u32 U64ToString(char* str, u64 n) {

    if(n == 0) {
        str[0] = (n % 10 + '0');
        return 1;
    }

    i32 i = 0;
    u64 m = n;
    while(m != 0) {
        m /= 10;
        i++;
    }
    
    u32 size = i--;
    for(; i > -1; i--) {
        str[i] = (n % 10 + '0');
        n /= 10;
    }
    return size;
}

void GLClearError() {
    while( glGetError() != GL_NO_ERROR );
}

bool GLLogCall(const char* function, const char* file, i32 line) {
    while(GLenum error = glGetError()) {

        const char* errorCode;
        switch (error) {
        case GL_INVALID_ENUM:
            errorCode = ("GL_INVALID_ENUM");
            break;
        case GL_INVALID_VALUE:
            errorCode = ("GL_INVALID_VALUE");
            break;
        case GL_INVALID_OPERATION:
            errorCode = ("GL_INVALID_OPERATION");
            break;
        case GL_STACK_OVERFLOW:
            errorCode = ("GL_STACK_OVERFLOW");
            break;
        case GL_STACK_UNDERFLOW:
            errorCode = ("GL_STACK_UNDERFLOW");
            break;
        case GL_OUT_OF_MEMORY:
            errorCode = ("GL_OUT_OF_MEMORY");
            break;
        case GL_INVALID_FRAMEBUFFER_OPERATION:
            errorCode = ("GL_INVALID_FRAMEBUFFER_OPERATION");
            break;
        case GL_CONTEXT_LOST:
            errorCode = ("GL_CONTEXT_LOST");
            break;
        case GL_TABLE_TOO_LARGE:
            errorCode = ("GL_TABLE_TOO_LARGE");
            break;
        default:
            std::cout << "OpenGL runtime error: " << error << " " << function << " " << line << " " << file << std::endl;
            break;
        }

        std::cout << "OpenGL runtime error: " << errorCode << " " << function << " " << line << " " << file << std::endl;
        return false;
    }
    return true;
}

void GLFWclearError() {
    const char* description;
    while( glfwGetError(&description) != GLFW_NO_ERROR );
}

bool GLFWlogCall(const char* function, const char* file, i32 line) {
    const char* description;
    while(auto error = glfwGetError(&description) ) {

        const char* errorCode;
        switch (error) {
            case GLFW_NO_ERROR:
                errorCode = ("GLFW_NO_ERROR");
                break;

            case GLFW_NOT_INITIALIZED:
                errorCode = ("GLFW_NOT_INITIALIZED");
                break;

            case GLFW_NO_CURRENT_CONTEXT:
                errorCode = ("GLFW_NO_CURRENT_CONTEXT");
                break;

            case GLFW_INVALID_ENUM:
                errorCode = ("GLFW_INVALID_ENUM");
                break;

            case GLFW_INVALID_VALUE:
                errorCode = ("GLFW_INVALID_VALUE");
                break;

            case GLFW_OUT_OF_MEMORY:
                errorCode = ("GLFW_OUT_OF_MEMORY");
                break;

            case GLFW_API_UNAVAILABLE:
                errorCode = ("GLFW_API_UNAVAILABLE");
                break;

            case GLFW_VERSION_UNAVAILABLE:
                errorCode = ("GLFW_VERSION_UNAVAILABLE");
                break;

            case GLFW_PLATFORM_ERROR:
                errorCode = ("GLFW_PLATFORM_ERROR");
                break;

            case GLFW_FORMAT_UNAVAILABLE :
                errorCode = ("GLFW_FORMAT_UNAVAILABLE");
                break;

            case GLFW_NO_WINDOW_CONTEXT  :
                errorCode = ("GLFW_NO_WINDOW_CONTEXT");
                break;
            default:
                std::cout << "GLFW runtime error: " << error << " " << function << " " << line << " " << file << " " << description << std::endl;
                break;
        }

        std::cout << "GLFW runtime error: " << errorCode << " " << function << " " << line << " " << file << " " << description << std::endl;
        return false;
    }
    return true;
}

template<typename T> T min(T t0 , T t1) {
    return t0 > t1 ? t1 : t0;
}
template<typename T> T max(T t0 , T t1) {
    return t0 > t1 ? t0 : t1;
}
void MemSet(void* dst, u8 v, u32 size) {
    for(u32 i = 0; i < size ; i++) {
        ((byte*)dst)[i] = v;
    }
}
void MemCpy(void* dst, const void* const src, u32 size) {
    for(u32 i = 0; i < size ; i++) {
        ((byte*)dst)[i] = ((byte*)src)[i];
    }
}

byte* base = nullptr;
struct MemoryBlockHeader {
    MemoryBlockHeader* left;
    MemoryBlockHeader* right;
    u32 size;
    bool is_free;
};
MemoryBlockHeader* search_free_block(u32 size) {

    MemoryBlockHeader* block = (MemoryBlockHeader*)(base);
    while(block) {
        if(block->is_free && block->size >= size) return block;
        block = block->right;
    }

    return nullptr;
}

void init_my_malloc(void* base_, u32 size) {
    base = (byte*)base_;
    MemoryBlockHeader* first_block = (MemoryBlockHeader*)base;
    first_block->is_free = true;
    first_block->left = 0;
    first_block->right = 0;
    first_block->size = size;
}
void* my_malloc(u32 size) {

    if(!size) return nullptr;

    MemoryBlockHeader* free_block = search_free_block(size);

#if DEBUG_BUILD == 1
    if(free_block->right) {
        ASSERT(free_block->right->left == free_block);
        ASSERT(free_block->right->size != 0);
    }
    if(free_block->left) {
        ASSERT(free_block->left->right == free_block);
        ASSERT(free_block->left->size != 0);
    }
    ASSERT(free_block->is_free);
#endif
    if(free_block) {
        free_block->is_free = false;
        if(free_block->size - size > sizeof(MemoryBlockHeader)) {

            byte* free_block_end = ((byte*)(free_block + 1)) + size;
            MemoryBlockHeader* new_free_block = (MemoryBlockHeader*)free_block_end;

            new_free_block->is_free = true;
            new_free_block->size = (free_block->size - size) - sizeof(MemoryBlockHeader);
            new_free_block->right = free_block->right;
            new_free_block->left = free_block;
            if(free_block->right) {
                free_block->right->left = new_free_block;
            }

            free_block->right = new_free_block;
            free_block->size = size;
        }
        return free_block + 1;
    }


    std::cerr << "out of memory" << std::endl;
    return nullptr;
}


void my_free(void* block) {
    if(!block) return;

    MemoryBlockHeader* header = ((MemoryBlockHeader*)block) - 1;
    ASSERT(!header->is_free);
    header->is_free = true;

    MemoryBlockHeader* next_block = header->right;
    MemoryBlockHeader* previous_block = header->left;

    while(next_block) {
        if(!next_block->is_free) break;

        header->size += next_block->size + sizeof(MemoryBlockHeader);
        header->right = next_block->right;
        if(header->right) header->right->left = header;

        next_block = header->right;
    }
    while(previous_block) {
        if(!previous_block->is_free) break;

        previous_block->size += header->size + sizeof(MemoryBlockHeader);;
        previous_block->right = header->right;

        if(previous_block->right) previous_block->right->left = previous_block;

        header = previous_block;
        previous_block = previous_block->left;
    }
}

void PrintBlocks() {
    MemoryBlockHeader* block = (MemoryBlockHeader*)(base);
    while(block) {
        std::cout << (u64)block << " " << block->is_free << " " << block->size << std::endl;
        block = block->right;
    }
}

void* my_malloc_debug(u32 size) {
    byte* mem = (byte*)my_malloc(size + 128);
    *((u32*)mem) = size;
    MemSet(mem + sizeof(u32), 255, 60);
    MemSet(mem + size + 64, 255, 64);
    return mem + 64;
}
void my_free_debug(void* mem) {
    if(!mem) return;
    u32 size = *((u32*)((byte*)mem - 64));
    byte* back_guard = ((byte*)mem) - 60;
    byte* front_guard = ((byte*)mem) + size;

    for(u32 i = 0; i < 60; i++) {
        LOG_ASSERT(back_guard[i] == 255, "Fatal Error: heap corruption");
    }
    for(u32 i = 0; i < 64; i++) {
        LOG_ASSERT(front_guard[i] == 255, "Fatal Error: heap corruption");
    }

    my_free(back_guard-4);
}
bool check_memory_integrity(void* mem) {
    if(!mem) return true;
    u32 size = *((u32*)((byte*)mem - 64));
    byte* back_guard = ((byte*)mem) - 60;
    byte* front_guard = ((byte*)mem) + size;

    bool corrupt = false;
    for(u32 i = 0; i < 60; i++) {
        corrupt |= back_guard[i] != 255;
    }
    for(u32 i = 0; i < 64; i++) {
        corrupt |= front_guard[i] != 255;
    }

    if(corrupt) {

        std::cout << "heap corruption" << std::endl;
        for(u32 i = 0; i < 60; i++) {
            std::cout << (u32)back_guard[i] << " ";
        }
        std::cout << std::endl;
        for(u32 i = 0; i < 64; i++) {
            std::cout << (u32)front_guard[i] << " ";
        }
        std::cout << std::endl;
    }

    ASSERT(!corrupt);
    return corrupt;
}
struct LinearAllocator {
    byte* mem;
    u32 bufferSize;

    void Init(void* mem_, u32 size) {
        mem = (byte*)mem_;
        bufferSize = size;
    }
    void* Allocate(u32 size) {
        auto tmp = mem;
        mem += size;
        return tmp;
    }
    void Free() {
        // ignore
    }
};

struct Button;
typedef void (*fnPtr)(Button*);

struct Pixel
{
    union {
        struct {
            u8 r, g, b, a;
        };
        u32 mem;
    };
};

struct Rectangle
{
    Pixel color;
    i32 leftbottomx;
    i32 leftbottomy;
    i32 righttopx;
    i32 righttopy;
};

template <typename T> struct DynamicBuffer {
    u32 size = 0;
    u32 capacity = 0;
    T* memoryPointer = nullptr;

    _FORCE_INLINE void Init() {
        static_assert(std::is_trivially_copyable<T>::value, "T must be trivially copyable");
        static_assert(std::is_trivially_destructible<T>::value, "T must be trivially destructible");

        memoryPointer = nullptr;
        size = 0;
        capacity = 0;
    }
    void Pushback(T e) {

        if (capacity < size + 1) {
            T* tmp = (T*)ALLOCATE(sizeof(T) * (size + 1) * 2);
            for (u32 i = 0; i < size; i++) {
                tmp[i] = memoryPointer[i];
            }
            FREE(memoryPointer);
            memoryPointer = tmp;
            capacity = (size + 1) * 2;
        }

        memoryPointer[size] = e;
        size++;
    }
    _FORCE_INLINE void Clear()
    {
        size = 0;
    }
    _FORCE_INLINE T& Back()
    {
        return memoryPointer[size - 1];
    }
    _FORCE_INLINE void Popback()
    {
        size--;
    }
    _FORCE_INLINE T& operator[](u32 index)
    {
        return memoryPointer[index];
    }
};



byte* ReadFileTerminated(const char* fileName, byte* buffer,u32* size_) {

    byte* sourceString;
    FILE* file = nullptr;
    file = fopen(fileName ,"r");
    if(file) {

        fseek(file , 0, SEEK_END);
        u32 size = ftell(file);
        fseek(file ,0, SEEK_SET);
        *size_ = size;

        sourceString = buffer;
        if(buffer == nullptr) {
            sourceString = (byte*)LOG(ALLOCATE(size + 1));
        }
        fread(sourceString , size , 1 , file);
        sourceString[size] = 0;

        fclose(file);
    }

    return sourceString;
}   

u32 StringHash(const char* str , u32 c) {
    u32 hash = 7;
    for(u32 i = 0; i < c ; i++) {
        hash = hash * 31 + str[i];
    }
    return hash;
}

u32 StrLen(const char* str) {
    u32 r = 0;
    while( str[r] != 0 ) r++;
    return r;
}
bool StrCmp(const char* str0, const char* str1) {
    ASSERT(str0 != nullptr && str1 != nullptr);
    while( *str0 == *str1 && *str0 != 0 && *str1 != 0 ) {str0++;str1++;}
    return *str0 == *str1;
}


template<typename K , typename V> struct HashNode {
    K key;
    V value;
};
template<typename K, typename V, u64 (*HASH_FUNCTION)(void*,K), bool (*EQ_FUNCTION)(void*,K,K), K INVALID_KEY> struct HashTable {
    HashNode<K,V>* array = nullptr;
    void* user;
    u32 cap = 0;
    u32 occupancy = 0;
    static constexpr f32 loadFactor = 0.5;

    void Init(void* user_) {
        user = user_;
        cap = 2;
        array = (HashNode<K,V>*)LOG(ALLOCATE( sizeof(HashNode<K,V>) * 2));
        array[0].key = INVALID_KEY;
        array[1].key = INVALID_KEY;
    }
    void CopyInit(HashTable<K,V,HASH_FUNCTION,EQ_FUNCTION,INVALID_KEY>* other) {

        MemCpy(this, other, sizeof(*this));
        array = (HashNode<K,V>*)LOG(ALLOCATE( sizeof(HashNode<K,V>) * other->cap));
        MemCpy(array, other->array, sizeof(HashNode<K,V>) * other->cap);
    }

    u32 Find(K key) {
    
        u32 index = HASH_FUNCTION(user, key) & (cap - 1);
        for(;;) {
            
            if(EQ_FUNCTION(user, array[index].key, key)) {
                return index;
            }
            index++;
            if(index == cap) {
                return ~u32(0);
            }
        }
    }

    void Delete(K key) {
        occupancy--;
        array[Find(key)].key = INVALID_KEY;
    }

    void Insert(K key , V val) {

        if( cap * loadFactor < (occupancy + 1) ) {
            GrowAndReHash();
        }

        occupancy++;
        u32 index = HASH_FUNCTION(user, key) & (cap - 1);
        for(;;) {
            if(EQ_FUNCTION(user, array[index].key, INVALID_KEY)) {
                array[index].key = key;
                array[index].value = val;
                return;
            }

            index++;
            if(index == cap) {
                GrowAndReHash();
                index = HASH_FUNCTION(user, key) & (cap - 1);
            }
        }
    }

    void GrowAndReHash() {

        u32 newCap = cap * 2;
        Begin:
        HashNode<K,V>* tmp = (HashNode<K,V>*)LOG(ALLOCATE(sizeof(HashNode<K,V>) * newCap));

        for(u32 i = 0; i < newCap ; i++) {
            tmp[i].key = INVALID_KEY;
        }
        for(u32 i = 0; i < cap; i++) {
            if(!EQ_FUNCTION(user, array[i].key, INVALID_KEY)) {

                u32 index = HASH_FUNCTION(user, array[i].key) & (newCap - 1);
                for(;;) {
                    if(EQ_FUNCTION(user, tmp[index].key, INVALID_KEY)) {
                        tmp[index].key = array[i].key;
                        tmp[index].value = array[i].value;
                        break;
                    }
                    index++;
                    if(index == newCap) {
                        newCap *= 2;
                        LOG(FREE(tmp));
                        goto Begin;
                    }
                }
            }
        }
        LOG(FREE(array));
        array = tmp;
        cap = newCap;
    }
    void Free() {
        LOG(FREE(array));
        array = nullptr;
    }
};

enum TokenType {
    TOKEN_EOF,
    TOKEN_IDENTIFIER,

    TOKEN_OPEN_PAREN,
    TOKEN_CLOSE_PAREN,
    TOKEN_OPEN_BRACKET,
    TOKEN_CLOSE_BRACKET,
    TOKEN_OPEN_BRACES,
    TOKEN_CLOSE_BRACES,

    TOKEN_EQUAL_SIGN,
    TOKEN_DOT,
    TOKEN_COMMA,
    TOKEN_COLON,
    TOKEN_SEMICOLON,

    TOKEN_STRING_LITERAL,
    TOKEN_NUMBER_LITERAL,
    TOKEN_NULL_LITERAL,
    
    TOKEN_UNKNOWN,
    TOKEN_COUNT,
};


struct Token {
    char*       text;
    u32         lenght;
    TokenType   type;
};

struct Tokenizer {
    char*   at;
    u32     line = 1;
};
const char* GetTokenStr(TokenType t) {
    switch (t) {
        case TOKEN_IDENTIFIER:                      return "TOKEN_IDENTIFIER";
        case TOKEN_OPEN_PAREN:                      return "TOKEN_OPEN_PAREN";
        case TOKEN_CLOSE_PAREN:                     return "TOKEN_CLOSE_PAREN";
        case TOKEN_OPEN_BRACKET:                    return "TOKEN_OPEN_BRACKET";
        case TOKEN_CLOSE_BRACKET:                   return "TOKEN_CLOSE_BRACKET";
        case TOKEN_OPEN_BRACES:                     return "TOKEN_OPEN_BRACES";
        case TOKEN_CLOSE_BRACES:                    return "TOKEN_CLOSE_BRACES";
        case TOKEN_DOT:                             return "TOKEN_DOT";
        case TOKEN_COMMA:                           return "TOKEN_COMA";
        case TOKEN_COLON:                           return "TOKEN_COLON";
        case TOKEN_SEMICOLON:                       return "TOKEN_SEMICOLON";
        case TOKEN_STRING_LITERAL:                  return "TOKEN_STRING_LITERAL";
        case TOKEN_NUMBER_LITERAL:                  return "TOKEN_NUMBER_LITERAL";
        case TOKEN_UNKNOWN:                         return "TOKEN_UNKNOWN";
        case TOKEN_EOF:                             return "TOKEN_EOF";
        case TOKEN_NULL_LITERAL:                    return "TOKEN_NULL_LITERAL";
        default:
            return nullptr;
            break;
    }
}
void PrintToken(Token t) {
    std::cout.write(t.text , t.lenght) << " ";
}


i64 GetI64(Token t) {
    char str[t.lenght+1]{0};
    for(u32 i = 0; i < t.lenght ; i++) {
        str[i] = t.text[i];
    }
    return atoi(str);
}
u64 GetU64(Token t) {
    char str[t.lenght+1]{0};
    for(u32 i = 0; i < t.lenght ; i++) {
        str[i] = t.text[i];
    }
    return strtoul(t.text , nullptr, 10);
}

f64 GetF64(Token t) {
    char str[t.lenght+1]{0};
    for(u32 i = 0; i < t.lenght ; i++) {
        str[i] = t.text[i];
    }
    return atof(str);
}

bool IsWhiteSpace(char c) {
    return  (c == ' ') ||
            (c == '\n') ||
            (c == '\t') ||
            (c == '\r');
}

u32 GetLineNumber(char* source ,char* at) {
    u32 c = 1;
    while( source != at ) {
        if( *source == '\n' ) c++;
        source++;
    }
    return c;
}
bool IsAlpha(char c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}
bool IsNumeric(char c) {
    return (c >= '0' && c <= '9');
}
bool TokenEquals(Token t , const char* match) {

    const char* m = match;
    for(u32 i = 0; i < t.lenght ; i++) {

        if(*m == 0 || t.text[i] != *m ) {
            return false;
        }

        m++;
    }
    return (*m == 0);
}
bool TokensEquals(Token t0 , Token t1) {

    if(t0.text == t1.text) {return true;}
    if(t0.lenght != t1.lenght) {return false;}

    for(u32 i = 0; i < t0.lenght ; i++) {
        if( t0.text[i] != t1.text[i] ) {
            return false;
        }
    }

    return true;
}
void EatMacors(Tokenizer* tokenizer) {

    bool eat = false;

    while(tokenizer->at[0]) {

        tokenizer->line += tokenizer->at[0] == '\n';
        if( tokenizer->at[0] == '\\' && tokenizer->at[1] == '\\' ) {
            eat = true;
            tokenizer->at++;
        }
        else if( tokenizer->at[0] == '\n') {
            if(eat) eat = false;
        }
        else {break;}

        ++tokenizer->at;
    }
}

void EatWhiteSpace(Tokenizer* tokenizer) {

    while(tokenizer->at[0]) {
        if( IsWhiteSpace(tokenizer->at[0])  ) {
            tokenizer->line += (tokenizer->at[0] == '\n');
            tokenizer->at++;
        }
        else if( tokenizer->at[0] == '/' && tokenizer->at[1] == '/' ) {
            tokenizer->at += 2;
            while( tokenizer->at[0] && !(tokenizer->at[0] == '\n' )) ++tokenizer->at;
            tokenizer->line++;
            tokenizer->at += 1;
        }
        else if( tokenizer->at[0] == '/' && tokenizer->at[1] == '*' ) {
            tokenizer->at += 2;
            while( tokenizer->at[0] && !(tokenizer->at[0] == '*' && tokenizer->at[1] == '/') ) tokenizer->line += (tokenizer->at++)[0] == '\n';
            tokenizer->at += 2;
        }
        else if( tokenizer->at[0] == '#') {
            EatMacors(tokenizer);
        }
        else {
            break;
        }
    }

}

Token GetToken(Tokenizer* tokenizer) {

    EatWhiteSpace(tokenizer);

    Token token{};
    token.lenght = 1;

    char c = tokenizer->at[0];
    token.text = tokenizer->at++; 

    switch (c) {
        case '\0'   :token.type = TOKEN_EOF;                break;
        case '('    :token.type = TOKEN_OPEN_PAREN;         break;
        case ')'    :token.type = TOKEN_CLOSE_PAREN;        break;
        case '['    :token.type = TOKEN_OPEN_BRACKET;       break;
        case ']'    :token.type = TOKEN_CLOSE_BRACKET;      break;
        case '{'    :token.type = TOKEN_OPEN_BRACES;        break;
        case '}'    :token.type = TOKEN_CLOSE_BRACES;       break;
        case ','    :token.type = TOKEN_COMMA;              break;
        case ':'    :token.type = TOKEN_COLON;              break;
        case ';'    :token.type = TOKEN_SEMICOLON;          break;
        case '='    :token.type = TOKEN_EQUAL_SIGN;         break;
        case '"'    :
            {
                token.text = tokenizer->at;
                token.type = TOKEN_STRING_LITERAL;

                while(  tokenizer->at[0] && tokenizer->at[0] != '"') {
                    
                    if(tokenizer->at[0] == '\\' && tokenizer->at[1] ) {
                        ++tokenizer->at;
                    }
                    ++tokenizer->at;
                }
                token.lenght = tokenizer->at - token.text;
                if( tokenizer->at[0] == '"' ) ++tokenizer->at;
                break;
            }

        case '.':
            if(!IsNumeric(tokenizer->at[0])) {token.type = TOKEN_DOT; break;}
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
            {
                bool e = true;
                bool point = (c == '.');
                token.type = TOKEN_NUMBER_LITERAL;
                while(IsNumeric(tokenizer->at[0]) || tokenizer->at[0] == '.') {

                    if(tokenizer->at[0] == '.' && !point) {
                        point = true;
                    }
                    else if( tokenizer->at[0] == '.' && point && e) {
                        e = false;
                        token.type = TOKEN_UNKNOWN;
                    }

                    tokenizer->at++;
                }
                if(tokenizer->at[0] == 'f') {
                    tokenizer->at++;

                    if( IsNumeric(tokenizer->at[0]) || tokenizer->at[0] == '.' ) {
                        token.type = TOKEN_UNKNOWN;
                    }
                }

                token.lenght = tokenizer->at - token.text;
                break;
            }
    
        
        default:
            if( IsAlpha(c) ) {
                token.type = TOKEN_IDENTIFIER;
                while( IsAlpha(tokenizer->at[0]) || IsNumeric(tokenizer->at[0]) || tokenizer->at[0] == '_' ) tokenizer->at++;

                token.lenght = tokenizer->at - token.text;

                if( TokenEquals(token , "null") ) {
                    token.type = TOKEN_NULL_LITERAL;
                }
            
            }
            else {
                token.type = TOKEN_UNKNOWN;
            }
            break;
    }

    return token;
}





enum BulletType
{
    PLAYER, ENEMY
};
struct Bullet
{
    Rectangle rect;
    i32 speedx;
    i32 speedy;
    BulletType type;
};
struct Texture
{
    i32 Width;
    i32 Height;
    Pixel* img;
};



Texture LoadTexture(const char* path, void* memory, u32* size_) {

    FILE* ImgHandle;
    ImgHandle = fopen(path, "r");
    if(!ImgHandle) {
        std::cout << "file could not be opened " << path << std::endl;
        return Texture{};
    }

    fseek(ImgHandle, 0, SEEK_END);
    u32 sizeofBmp = ftell(ImgHandle);
    fseek(ImgHandle, 0, SEEK_SET);
    byte* mem = (byte*)ALLOCATE(sizeofBmp);
    fread(mem, sizeofBmp, 1, ImgHandle);
    fclose(ImgHandle);

    if (!(((char*)mem)[0] == 'B' && ((char*)mem)[1] == 'M')) {
        FREE(mem);
        std::cout << "not a bmp file" << std::endl;
        return Texture{};
    }
    if (*((u32*)(mem + 30)) != 0) {
        FREE(mem);
        std::cout << "error image compressed" << std::endl;
        return Texture{};
    }

    u32 imageOffSet = *((u32*)(mem + 10));
    i32 imageWidth = *((i32*)(mem + 18));
    i32 imageHeight = *((i32*)(mem + 22));
    Texture texture;

    if (memory == nullptr) {
        Pixel* img = (Pixel*)ALLOCATE(imageHeight * imageWidth * 4);
        texture.img = img;
    }
    else {
        *size_ = imageHeight * imageWidth * 4;
        Pixel* img = (Pixel*)memory;
        texture.img = img;
    }

    texture.Height = imageHeight;
    texture.Width = imageWidth;
    u32 rowOffset = 0;

    if (imageWidth * 3 % 4 != 0)
    {
        rowOffset = 4 - (imageWidth * 3 % 4);
    }

    for (i32 i = 0; i < imageHeight; i++)
    {
        for (i32 k = 0; k < imageWidth; k++)
        {
            texture.img[i * imageWidth + k].b = (mem + imageOffSet)[(imageWidth * 3 + rowOffset) * i + k * 3 + 0];
            texture.img[i * imageWidth + k].g = (mem + imageOffSet)[(imageWidth * 3 + rowOffset) * i + k * 3 + 1];
            texture.img[i * imageWidth + k].r = (mem + imageOffSet)[(imageWidth * 3 + rowOffset) * i + k * 3 + 2];
            texture.img[i * imageWidth + k].a = 0;

            texture.img[i * imageWidth + k].a = (texture.img[i * imageWidth + k].mem == 0 ? 0 : 255);

        }
    }
    FREE(mem);

    return texture;
}

u32 getBmpImageSize(const char* path) {
    FILE* ImgHandle;
    i32 mem[2];
    ImgHandle = fopen(path, "r");
    if (!ImgHandle) {
        std::cout << "file could not be opened " << path << std::endl;
    }
    fseek(ImgHandle, 18, SEEK_SET);
    fread(mem, 8, 1, ImgHandle);
    
    fseek(ImgHandle, 0, SEEK_SET);
    fclose(ImgHandle);
    return mem[0] * mem[1] * 4;
}

/*
struct Animation {

    Pixel* frames;
    u32 picHeight, picWidth;
    u32 bufferSize;
    u16 frameCount;
    u16 counter;

    void loadManyTextures(u32 size, const char** images)
    {
        frameCount = size;
        i32 totalMemorySize = 0;
        u32 firstImageSize = 0;
        u32 currentImageSize = 0;
        for (i32 i = 0; i < size; i++)
        {
            currentImageSize = getBmpImageSize(images[i]);
            totalMemorySize += currentImageSize;
            if (i == 0)
            {
                firstImageSize = currentImageSize;
            }
            LOGASSERT(currentImageSize == firstImageSize, " Animation images must be equal in dimensions ");
        }
        bufferSize = totalMemorySize + sizeof(Texture);
        byte* location = (byte*)ALLOCATE(bufferSize);
        for (i32 k = 0; k < size; k++)
        {
            Texture* img;
            img = LoadTexture(images[k], (location + k*firstImageSize), nullptr);
            LOGASSERT(img->Height * img->Width * 4 == firstImageSize, " Animation ");
            picHeight = img->Height;
            picWidth = img->Width;
        }
        frames = (Pixel*)location;
    }
    
};
*/


struct GameState;
void DrawTextureRectTransparent(GameState* state, Rectangle rect, Texture* texture, bool reverseY, bool reverseX);

/*
struct Button {
    Animation animation;
    Rectangle rectangle;
    u64 lastPressed;

    fnPtr function;
};
*/
/*
void DrawButton(Button* button, GameState* state) {
    Texture texture;
    texture.Height = button->animation.picHeight;
    texture.Width = button->animation.picWidth;
    texture.img = button->animation.frames + (button->animation.counter) * button->animation.picHeight * button->animation.picWidth;
    DrawTextureRectTransparent(state, button->rectangle, &texture, false, false);
}
*/
struct Player {
    Rectangle rect;
    i32 HP;
    f32 Speedx;
    f32 Speedy;
    f32 accelerationx;
    f32 accelerationy;
    u64 lastShootingTime;
};

bool RectCollision(Rectangle rect1, Rectangle rect2)
{
    return ((rect1.righttopx > rect2.leftbottomx &&
        rect1.righttopx < rect2.righttopx) &&
        (rect1.righttopy > rect2.leftbottomy &&
            rect1.righttopy < rect2.righttopy)) ||

        ((rect1.leftbottomx > rect2.leftbottomx &&
            rect1.leftbottomx < rect2.righttopx) &&
            (rect1.leftbottomy > rect2.leftbottomy &&
                rect1.leftbottomy < rect2.righttopy));
}

bool IsInsideRect(Rectangle rect, i32 mouseX, i32 mouseY)
{
    return ((mouseX > rect.leftbottomx)
            && (mouseX < rect.righttopx)
            && (mouseY > rect.leftbottomy)
            && (mouseY < rect.righttopy));
}

struct Enemies
{
    Rectangle rectangle;
    f32 speedx = 0;
    f32 speedy = 0;
    i32 HP;
    u32 orderNumber = 0;
    u32 lastShootingTime = 0;
};

struct Layer
{
    f32 Speed;
    u32 Size;
    Rectangle Stars[20];
};

enum GameStateEnum : u32 {
    
    GAME_STATE_NONE,

    GAME_STATE_MENU,
    GAME_STATE_PLAYING,
    GAME_STATE_WON,
    GAME_STATE_GAME_OVER,

    GAME_STATE_COUNT,
};
enum KeyBit {
    KEY_NONE,

    KEY_ESC         = 1 << 0,
    KEY_SPACE       = 1 << 1,
    KEY_ENTER       = 1 << 2,
    KEY_W           = 1 << 3,
    KEY_A           = 1 << 4,
    KEY_S           = 1 << 5,
    KEY_D           = 1 << 6,
    KEY_LEFT_MOUSE  = 1 << 7,

    KEY_COUNT,
};

struct GameConfig {
    char* playerTexture;
    char* enemyTexture;

    char* gameOverTexture;
    char* menuTexture;
    char* gameWinTexture;
};

typedef u64 KeyBoardState;
struct GameState {

    GameConfig cfg;
    GLFWwindow* window;
    Layer* layers;
    Pixel* FrameBuffer;

    LinearAllocator linAllocator;
    Texture playerTexture;
    Texture enemyTexture;
    Texture gameOverTexture;
    Texture menuTexture;
    Texture winTexture;

    KeyBoardState keys;
    Player player;
    u64 tick;

    f64 mouseX;
    f64 mouseY;

    DynamicBuffer<Enemies> Enemy;
    DynamicBuffer<Bullet> Bullets;
    //DynamicBuffer<Button> Buttons;

    i32 killCount;
    u32 level;
    u32 HEIGHT;
    u32 WIDTH;

    GameStateEnum currentState;
    bool shouldRun;
};

void DrawRectangle(GameState* state, Rectangle rect) {
    for (; rect.leftbottomy < rect.righttopy; rect.leftbottomy++) {
        i32 temp = rect.leftbottomx;
        for (; rect.leftbottomx < rect.righttopx; rect.leftbottomx++) {
            if (!(rect.leftbottomx + (rect.leftbottomy * state->WIDTH) < 0 || rect.leftbottomx + (rect.leftbottomy * state->WIDTH) > state->HEIGHT * state->WIDTH)) {
                state->FrameBuffer[rect.leftbottomx + (rect.leftbottomy * state->WIDTH)] = rect.color;
            }
        }
        rect.leftbottomx = temp;
    }
}

void Clear(GameState* state) {
    for (i32 i = 0; i < state->HEIGHT * state->WIDTH; i++) {
        state->FrameBuffer[i].r = 0;
        state->FrameBuffer[i].g = 0;
        state->FrameBuffer[i].b = 0;
        state->FrameBuffer[i].a = 0;
    }
}
void DrawBgk(GameState* state) {
    for (i32 k = 0; k < 3; k++) {
        for (i32 i = 0; i < 20; i++) {

            DrawRectangle(state, state->layers[k].Stars[i]);
        }
    }
}
void DrawTextureRectTransparent(GameState* state, Rectangle rect, Texture* texture, bool reverseY, bool reverseX) {

    f32 sizex = rect.righttopx - rect.leftbottomx;
    f32 sizey = rect.righttopy - rect.leftbottomy;

    f32 dx = 1.f / sizex;
    f32 dy = 1.f / sizey;
    f32 u = 0;
    f32 v = 0;

    Pixel tint = rect.color;
    i32 x0 = rect.leftbottomx;
    i32 y0 = rect.leftbottomy;
    i32 x1 = rect.righttopx;
    i32 y1 = rect.righttopy;
    for (; y0 < y1; y0++) {
        for (x0 = rect.leftbottomx; x0 < x1; x0++) {
            if (x0 + (y0 * state->WIDTH) >= 0 && x0 + (y0 * state->WIDTH) < state->HEIGHT * state->WIDTH) {
                u32 xCoord;
                u32 yCoord;
                if (reverseX) xCoord = (1 - u) * texture->Width;  else xCoord = u * texture->Width;
                if (reverseY) yCoord = (1 - v) * texture->Height; else yCoord = v * texture->Height;
                u32 index = xCoord + yCoord * texture->Width;


                Pixel tex = texture->img[index];
                tex.r = (tex.r * tint.r) >> 8;
                tex.g = (tex.g * tint.g) >> 8;
                tex.b = (tex.b * tint.b) >> 8;
                tex.a = (tex.a * tint.a) >> 8;

                Pixel fbo = state->FrameBuffer[x0 + (y0 * state->WIDTH)];
                Pixel n;

                n.r = (((255 - tex.a) * fbo.r) >> 8) + ((tex.a * tex.r) >> 8);
                n.g = (((255 - tex.a) * fbo.g) >> 8) + ((tex.a * tex.g) >> 8);
                n.b = (((255 - tex.a) * fbo.b) >> 8) + ((tex.a * tex.b) >> 8);
                n.a = (((255 - tex.a) * fbo.a) >> 8) + ((tex.a * tex.a) >> 8);

                state->FrameBuffer[x0 + (y0 * state->WIDTH)] = n;
            }
            u += dx;
        }
        u = 0;
        v += dy;
    }
}


void DrawTextureRectFilter(GameState* state, Rectangle rect, Texture* texture) {

    f32 sizex = rect.righttopx - rect.leftbottomx;
    f32 sizey = rect.righttopy - rect.leftbottomy;

    f32 dx = 1.f / sizex;
    f32 dy = 1.f / sizey;
    f32 u = 0;
    f32 v = 0;

    i32 x0 = state->player.rect.leftbottomx;
    i32 y0 = state->player.rect.leftbottomy;
    i32 x1 = state->player.rect.righttopx;
    i32 y1 = state->player.rect.righttopy;
    for (; y0 < y1; y0++) {
        for (x0 = state->player.rect.leftbottomx; x0 < x1; x0++) {
            if(x0 + (y0 * state->WIDTH) >= 0 && x0 + (y0 * state->WIDTH) < state->HEIGHT * state->WIDTH) {
                u32 xCoord = u * texture->Width;
                u32 yCoord = v * texture->Height;

                u32 index = xCoord + (yCoord) * texture->Width;
                Pixel t0 = texture->img[index];

                index = (xCoord+1) + yCoord * texture->Width;
                Pixel t1 = texture->img[index];

                index = (xCoord+1) + (yCoord-1) * texture->Width;
                Pixel t2 = texture->img[index];

                index = (xCoord) + (yCoord-1) * texture->Width;
                Pixel t3 = texture->img[index];

                Pixel q = {(t0.mem + t1.mem) >> 1 + (t2.mem + t3.mem) >> 1};
                state->FrameBuffer[x0 + (y0 * state->WIDTH)] = q;
            }
            u += dx;
        }
        u = 0;
        v += dy;
    }
}
void DrawTextureRect(GameState* state, Rectangle rect, Texture* texture) {

    f32 sizex = rect.righttopx - rect.leftbottomx;
    f32 sizey = rect.righttopy - rect.leftbottomy;

    f32 dx = 1.f / sizex;
    f32 dy = 1.f / sizey;
    f32 u = 0;
    f32 v = 0;

    i32 x0 = state->player.rect.leftbottomx;
    i32 y0 = state->player.rect.leftbottomy;
    i32 x1 = state->player.rect.righttopx;
    i32 y1 = state->player.rect.righttopy;
    for (; y0 < y1; y0++) {
        for (x0 = state->player.rect.leftbottomx; x0 < x1; x0++) {
            if (x0 + (y0 * state->WIDTH) >= 0 && x0 + (y0 * state->WIDTH) < state->HEIGHT * state->WIDTH) {
                u32 xCoord = u * texture->Width;
                u32 yCoord = v * texture->Height;
                u32 index = xCoord + yCoord * texture->Width;
                state->FrameBuffer[x0 + (y0 * state->WIDTH)] = texture->img[index];
            }
            u += dx;
        }
        u = 0;
        v += dy;
    }
}

void Input(GameState* state)
{
    if ( (state->keys & KEY_SPACE) && state->tick - state->player.lastShootingTime > 50) {
        state->player.lastShootingTime = state->tick;
        Bullet bullet;
        bullet.speedx = state->player.Speedx;
        bullet.speedy = state->player.Speedy + 7;
        i32 xcenter = state->player.rect.righttopx - state->player.rect.leftbottomx;
        bullet.rect = { Pixel{255, 0, 0, 255}, xcenter / 2 + state->player.rect.leftbottomx, state->player.rect.righttopy, (xcenter / 2 + state->player.rect.leftbottomx) + 6, (state->player.rect.righttopy) + 14 };
        state->Bullets.Pushback(bullet);
    }
}

void UpdateBgk(GameState* state) {
    for (i32 k = 0; k < 3; k++)
    {
        for (i32 i = 0; i < 20; i++)
        {
            state->layers[k].Stars[i].leftbottomy -= state->layers[k].Speed;
            state->layers[k].Stars[i].righttopy -= state->layers[k].Speed;
            if (state->layers[k].Stars[i].leftbottomy < 0)
            {
                state->layers[k].Stars[i].leftbottomy = state->HEIGHT;
                state->layers[k].Stars[i].righttopy = state->layers[k].Stars[i].leftbottomy + rand() % 5 + 10;
                state->layers[k].Stars[i].leftbottomx = rand() % state->WIDTH;
                state->layers[k].Stars[i].righttopx = state->layers[k].Stars[i].leftbottomx + rand() % 5 + 10;
            }
        }
    }
}
void UpdatePlayer(GameState* state) {
    constexpr f64 PlayerAcceleration = 0.5;

    bool w = (state->keys & KEY_W) == KEY_W;
    bool a = (state->keys & KEY_A) == KEY_A;
    bool s = (state->keys & KEY_S) == KEY_S;
    bool d = (state->keys & KEY_D) == KEY_D;

    state->player.accelerationx = (PlayerAcceleration * d) - (PlayerAcceleration * a);
    state->player.accelerationy = (PlayerAcceleration * w) - (PlayerAcceleration * s);
    state->player.Speedx += state->player.accelerationx;
    state->player.Speedy += state->player.accelerationy;

    if (state->player.Speedx > 5) {
        state->player.Speedx = 5;
    }
    else if (state->player.Speedx < -5) {
        state->player.Speedx = -5;
    }
    if (state->player.Speedy > 5) {
        state->player.Speedy = 5;
    }
    else if (state->player.Speedy < -5) {

        state->player.Speedy = -5;
    }

    state->player.rect.leftbottomx = i32(state->player.rect.leftbottomx) + i32(state->player.Speedx);
    state->player.rect.leftbottomy = i32(state->player.rect.leftbottomy) + i32(state->player.Speedy);
    state->player.rect.righttopx = i32(state->player.rect.righttopx) + i32(state->player.Speedx);
    state->player.rect.righttopy = i32(state->player.rect.righttopy) + i32(state->player.Speedy);
    if (state->player.rect.leftbottomx + state->player.Speedx <= 0 || state->player.rect.righttopx >= state->WIDTH - state->player.Speedx)
    {
        state->player.Speedx = state->player.Speedx * -0.5;
    }
    if (state->player.rect.leftbottomy + state->player.Speedy <= 0 || state->player.rect.righttopy >= state->HEIGHT - state->player.Speedy)
    {
        state->player.Speedy = state->player.Speedy * -0.5;
    }
    if (state->player.HP == 0)
    {
        state->currentState = GAME_STATE_GAME_OVER;
        std::cout << "Game Over! :|" << std::endl;
    }
}
const f32 trig_loop_up[60] = {
    cos(0 * 0.209), sin(0 * 0.209),
    cos(1 * 0.209), sin(1 * 0.209),
    cos(2 * 0.209), sin(2 * 0.209),
    cos(3 * 0.209), sin(3 * 0.209),
    cos(4 * 0.209), sin(4 * 0.209),
    cos(5 * 0.209), sin(5 * 0.209),
    cos(6 * 0.209), sin(6 * 0.209),
    cos(7 * 0.209), sin(7 * 0.209),
    cos(8 * 0.209), sin(8 * 0.209),
    cos(9 * 0.209), sin(9 * 0.209),
    cos(10 * 0.209), sin(10 * 0.209),
    cos(11 * 0.209), sin(11 * 0.209),
    cos(12 * 0.209), sin(12 * 0.209),
    cos(13 * 0.209), sin(13 * 0.209),
    cos(14 * 0.209), sin(14 * 0.209),
    cos(15 * 0.209), sin(15 * 0.209),
    cos(16 * 0.209), sin(16 * 0.209),
    cos(17 * 0.209), sin(17 * 0.209),
    cos(18 * 0.209), sin(18 * 0.209),
    cos(19 * 0.209), sin(19 * 0.209),
    cos(20 * 0.209), sin(20 * 0.209),
    cos(21 * 0.209), sin(21 * 0.209),
    cos(22 * 0.209), sin(22 * 0.209),
    cos(23 * 0.209), sin(23 * 0.209),
    cos(24 * 0.209), sin(24 * 0.209),
    cos(25 * 0.209), sin(25 * 0.209),
    cos(26 * 0.209), sin(26 * 0.209),
    cos(27 * 0.209), sin(27 * 0.209),
    cos(28 * 0.209), sin(28 * 0.209),
    cos(29 * 0.209), sin(29 * 0.209),
};

void Update(GameState* state) {
    UpdatePlayer(state);
    UpdateBgk(state);


    for (i32 i = 0; i < state->Enemy.size; i++) {
        bool remove = false;
        switch (state->level)
        {
        case 0:
        case 1:
        case 2:
        {
            u32 centerx = state->Enemy[i].rectangle.righttopx - state->Enemy[i].rectangle.leftbottomx;
            if (state->Enemy[i].lastShootingTime == 0)
            {
                Bullet bullet;
                bullet.speedx = 0;
                bullet.speedy = -7;
                bullet.rect.righttopx = (centerx / 2) + state->Enemy[i].rectangle.leftbottomx;
                bullet.rect.righttopy = state->Enemy[i].rectangle.leftbottomy;
                bullet.rect.leftbottomx = bullet.rect.righttopx - 6;
                bullet.rect.leftbottomy = bullet.rect.righttopy - 14;
                state->Enemy[i].lastShootingTime = rand() % 130 + 50;
                bullet.rect.color = { 0, 255, 0, 255 };
                state->Bullets.Pushback(bullet);
            }
            state->Enemy[i].lastShootingTime--;
            if (state->Enemy[i].HP == 0)
            {
                remove = true;
                state->killCount++;
            }
            state->Enemy[i].rectangle.leftbottomx += state->Enemy[i].speedx;
            state->Enemy[i].rectangle.leftbottomy += state->Enemy[i].speedy;
            state->Enemy[i].rectangle.righttopx += state->Enemy[i].speedx;
            state->Enemy[i].rectangle.righttopy += state->Enemy[i].speedy;
            if (state->Enemy[i].rectangle.leftbottomx < 0 || state->Enemy[i].rectangle.righttopx > state->WIDTH)
            {
                state->Enemy[i].speedx = state->Enemy[i].speedx * -1;
            }
            if (state->Enemy[i].rectangle.leftbottomy < 0 || state->Enemy[i].rectangle.righttopy > state->HEIGHT)
            {
                state->Enemy[i].speedx = state->Enemy[i].speedy * -1;
            }
            if (state->Enemy[i].rectangle.leftbottomy <= state->HEIGHT / 2 || state->Enemy[i].rectangle.righttopy >= state->HEIGHT)
            {
                state->Enemy[i].speedy = state->Enemy[i].speedy * -1;
            }
            break;
        }
        case 3:
        {

            if (state->Enemy[i].lastShootingTime == 0)
            {
                Bullet bullet;
                u32 sizex = (state->Enemy[i].rectangle.righttopx - state->Enemy[i].rectangle.leftbottomx);
                u32 sizey = (state->Enemy[i].rectangle.righttopy - state->Enemy[i].rectangle.leftbottomy);
                u32 centerx = state->Enemy[i].rectangle.leftbottomx + (sizex / 2);
                u32 centery = state->Enemy[i].rectangle.leftbottomy + (sizey / 2);
                f32 rad = sqrt(sizex * sizex + sizey + sizey);

                state->Enemy[i].lastShootingTime = rand() % 200 + 50;
                for (i32 k = 0; k < 30; k++)
                {
                    f32 cosK = trig_loop_up[k << 1];
                    f32 sinK = trig_loop_up[(k << 1) + 1];
                    
                    bullet.speedx = /*cos(k * 0.209)*/ cosK * 7;
                    bullet.speedy = /*sin(k * 0.209)*/ sinK * -7;

                    bullet.rect.righttopx = centerx + i32(cosK * rad * 1.1);
                    bullet.rect.righttopy = centery + i32(sinK * -rad * 1.1);
                    bullet.rect.leftbottomx = bullet.rect.righttopx - 6;
                    bullet.rect.leftbottomy = bullet.rect.righttopy - 14;
                    bullet.rect.color = { 0, 255, 0, 255 };
                    state->Bullets.Pushback(bullet);
                }
            }
            state->Enemy[i].lastShootingTime--;
            if (state->Enemy[i].HP == 0)
            {
                remove = true;
                state->killCount++;
            }
            state->Enemy[i].rectangle.leftbottomx += state->Enemy[i].speedx;
            state->Enemy[i].rectangle.leftbottomy += state->Enemy[i].speedy;
            state->Enemy[i].rectangle.righttopx += state->Enemy[i].speedx;
            state->Enemy[i].rectangle.righttopy += state->Enemy[i].speedy;

            if (state->Enemy[i].rectangle.leftbottomx < 0 || state->Enemy[i].rectangle.righttopx > state->WIDTH)
            {
                state->Enemy[i].speedx *= -1;
            }
            if (state->Enemy[i].rectangle.leftbottomy < 0 || state->Enemy[i].rectangle.righttopy > state->HEIGHT || state->Enemy[i].rectangle.leftbottomy < (state->HEIGHT / 2))
            {
                state->Enemy[i].speedy *= -1;
            }
            break;
        }
        }
        if (remove == true)
        {
            if (i == state->Enemy.size - 1)
            {
                state->Enemy.Popback();
            }
            else
            {
                state->Enemy[i] = state->Enemy.Back();
                state->Enemy.Popback();
            }
        }

    }
    if (state->Enemy.size == 0)
    {
        switch (state->level) {
        case 0:
        {
            for (i32 i = 0; i < state->killCount + 1; i++)
            {
                Enemies enemy1;
                enemy1.speedx = 1;
                enemy1.HP = 1;
                enemy1.speedy = 0;
                i32 randPosX = (rand() % (state->WIDTH - 200)) + 100; // sloowwww....
                i32  randPosY = (rand() % (state->HEIGHT - 600)) + 500;
                enemy1.lastShootingTime = rand() % 50 + 50;
                enemy1.rectangle = { Pixel{ 255, 255, 255, 255 }, randPosX, randPosY , randPosX + 100 ,  randPosY + 100 };
                state->Enemy.Pushback(enemy1);
            }
            if (state->killCount == 2) 
            {
                state->level++;
                state->killCount = -3;
            }
            else
            {
                state->killCount = 0;
            }
            break;
        }
        case 1:
        {

            for (i32 i = 0; i < state->killCount + 1; i++)
            {
                Enemies enemy1;
                enemy1.speedx = 2;
                enemy1.HP = 1;
                enemy1.speedy = 0;
                i32 randPosX = (rand() % (state->WIDTH - 200)) + 100;
                i32  randPosY = (rand() % (state->HEIGHT - 600)) + 500;
                enemy1.lastShootingTime = rand() % 30 + 50;
                enemy1.rectangle = { Pixel{ 255, 255, 255, 255 }, randPosX, randPosY , randPosX + 75 ,  randPosY + 75 };
                state->Enemy.Pushback(enemy1);

            }
            if (state->killCount == 2)
            {
                state->level++;
                state->killCount = -3;
            }
            else
            {
                state->killCount = 0;
            }
            break;
        }
        case 2:
        {
            for (i32 i = 0; i < state->killCount + 1; i++)
            {
                Enemies enemy1;
                enemy1.speedx = 1.5;
                enemy1.HP = 1;
                enemy1.speedy = 2;
                i32 randPosX = (rand() % (state->WIDTH - 200)) + 100;
                i32 randPosY = (rand() % (state->HEIGHT - 600)) + 500;
                enemy1.lastShootingTime = rand() % 30 + 50;
                enemy1.rectangle = { Pixel{ 255, 255, 255, 255 }, randPosX, randPosY , randPosX + 75 ,  randPosY + 75 };
                state->Enemy.Pushback(enemy1);

            }
            if (state->killCount == 2)
            {
                state->Enemy.Clear();
                state->level++;
            }
            else {
                state->killCount = 0;
            }

            break;
        }
        case 3:
        {

            Enemies enemy1;
            enemy1.speedx = 1.5;
            enemy1.speedy = 1.5;
            enemy1.HP = 10;
            i32 randPosX = rand() % (state->WIDTH - 150);
            i32  randPosY = (rand() % (state->HEIGHT / 2 - 150) + (state->HEIGHT / 2));
            enemy1.lastShootingTime = rand() % 30 + 50;
            enemy1.rectangle = { Pixel{ 255, 255, 255, 255 }, randPosX, randPosY, randPosX + 150 , randPosY + 150 };
            state->Enemy.Pushback(enemy1);
            if (state->killCount == 1)
            {
                state->currentState = GAME_STATE_WON;
                std::cout << "You beat the game" << std::endl;
            }
            state->killCount = 0;
            break;
        }
        }
    }
    for (i32 i = 0; i < state->Bullets.size; i++)
    {
        state->Bullets[i].rect.leftbottomx += state->Bullets[i].speedx;
        state->Bullets[i].rect.leftbottomy += state->Bullets[i].speedy;
        state->Bullets[i].rect.righttopx += state->Bullets[i].speedx;
        state->Bullets[i].rect.righttopy += state->Bullets[i].speedy;
        bool remove = false;
        if (state->Bullets[i].rect.righttopx < 0 || state->Bullets[i].rect.leftbottomx > state->WIDTH || state->Bullets[i].rect.righttopy < 0 || state->Bullets[i].rect.leftbottomy > state->HEIGHT)
        {
            remove = true;
        }
        if (RectCollision(state->Bullets[i].rect, state->player.rect))
        {
            state->player.HP--;
            remove = true;
        }
        else
        {
            for (i32 k = 0; k < state->Enemy.size; k++)
            {
                if (RectCollision(state->Bullets[i].rect, state->Enemy[k].rectangle))
                {
                    state->Enemy[k].HP--;
                    remove = true;
                    break;
                }
            }
        }
        if (remove == true)
        {

            if (i == state->Bullets.size - 1)
            {
                state->Bullets.Popback();
            }
            else
            {
                state->Bullets[i] = state->Bullets.Back();
                state->Bullets.Popback();
            }
        }
    }
}

void DrawPlayinState(GameState* state) {
    Clear(state);
    DrawBgk(state);
    for (i32 i = 0; i < state->Bullets.size; i++) {
        DrawRectangle(state, state->Bullets[i].rect);
    }
    for (i32 i = 0; i < state->Enemy.size; i++) {
        DrawTextureRectTransparent(state, state->Enemy[i].rectangle, &state->enemyTexture, true, false);
    }
    DrawTextureRectTransparent(state, state->player.rect, &state->playerTexture, false, false);
}

void UpdateMenuState(GameState* state) {
    UpdateBgk(state);
    GLFW_CALL(glfwGetCursorPos(state->window, &state->mouseX, &state->mouseY));
    
    state->mouseY = state->HEIGHT - state->mouseY;
    GLFW_CALL(i32 cursosButtonDown = glfwGetMouseButton(state->window, GLFW_MOUSE_BUTTON_1));

    /*
    for (i32 k = 0; k < state->Buttons.size; k++){
        
        bool cursosColision = IsInsideRect(state->Buttons[k].rectangle, state->mouseX, state->mouseY);
        if (cursosColision && cursosButtonDown && (state->tick - state->Buttons[k].lastPressed >= 40)) {
            state->Buttons[k].lastPressed = state->tick;
        }
    }
    */
}
void DrawMenuState(GameState* state) {
    Clear(state);
    DrawBgk(state);
    DrawTextureRectTransparent(state, Rectangle{ {255,255,255,255}, 0,0, (i32)state->WIDTH / 2,(i32)state->HEIGHT / 2}, &state->menuTexture, false, false);

    /*
    for (i32 i = 0; i < state->Buttons.size; i++) {
        DrawButton(state->Buttons.memoryPointer+i, state);
    }
    */
}
Token PeekToken(Tokenizer peek) {
    return GetToken(&peek);
}

u64 HashToken(void* user, Token t) {
    return StringHash(t.text, t.lenght);
}
bool EQToken(void* user, Token t0, Token t1) {
    if(t0.text == t1.text) {return true;}
    if(t0.lenght != t1.lenght) {return false;}

    for(u32 i = 0; i < t0.lenght ; i++) {
        if( t0.text[i] != t1.text[i] ) {
            return false;
        }
    }
    return true;
}

bool Parse(GameState* state, Tokenizer* tokenizer, HashTable<Token, Token, &HashToken, &EQToken, Token{}>* symbols, bool* error) {

    Token peek = GetToken(tokenizer);
    switch(peek.type) {
    case TOKEN_EOF:return false;
    case TOKEN_IDENTIFIER:
        {
            
            Token eq = GetToken(tokenizer);
            if(eq.type != TOKEN_EQUAL_SIGN) {std::cout << "expected = at line: " << tokenizer->line << std::endl; *error = true;}
            Token val = GetToken(tokenizer);
            if(val.type != TOKEN_STRING_LITERAL && val.type != TOKEN_NUMBER_LITERAL) {std::cout << "expected string or number at line: " << tokenizer->line << std::endl; *error = true;}
            Token semi = GetToken(tokenizer);
            if(semi.type != TOKEN_SEMICOLON) {std::cout << "expected ; at line: " << tokenizer->line << std::endl; *error = true;}

            u32 index = symbols->Find(peek);
            if(index != ~u32(0) ) {std::cout << "symbol redefinition at line: " << tokenizer->line << std::endl; *error = true;}
            else {
                symbols->Insert(peek, val);
            }
            break;
        }
    }

    return true;
}


void ParseConfigFile(GameState* state, Tokenizer tokenizer) {

    HashTable<Token, Token, &HashToken, &EQToken, Token{}> symbols;
    symbols.Init(nullptr);

    bool error = false;
    while(Parse(state, &tokenizer, &symbols, &error));

    Token key;
    key.text = const_cast<char*>("window_width");
    key.lenght = sizeof("window_width")-1;
    u32 index = symbols.Find(key);
    if(index != ~u32(0)) {
        Token val = symbols.array[index].value;
        state->WIDTH = GetI64(val);
    }
    key.text = const_cast<char*>("window_height");
    key.lenght = sizeof("window_height")-1;
    index = symbols.Find(key);
    if(index != ~u32(0)) {
        Token val = symbols.array[index].value;
        state->HEIGHT = GetI64(val);
    }

    key.text = const_cast<char*>("player_hp");
    key.lenght = sizeof("player_hp")-1;
    index = symbols.Find(key);
    if(index != ~u32(0)) {
        Token val = symbols.array[index].value;
        state->player.HP = GetI64(val);
    }

    key.text = const_cast<char*>("player_texture");
    key.lenght = sizeof("player_texture")-1;
    index = symbols.Find(key);
    if(index != ~u32(0)) {
        Token val = symbols.array[index].value;
        state->cfg.playerTexture = (char*)state->linAllocator.Allocate(val.lenght+1);
        state->cfg.playerTexture[val.lenght] = 0;
        MemCpy(state->cfg.playerTexture, val.text, val.lenght);
    }

    key.text = const_cast<char*>("enemy_texture");
    key.lenght = sizeof("enemy_texture")-1;
    index = symbols.Find(key);
    if(index != ~u32(0)) {
        Token val = symbols.array[index].value;
        state->cfg.enemyTexture = (char*)state->linAllocator.Allocate(val.lenght+1);
        state->cfg.enemyTexture[val.lenght] = 0;
        MemCpy(state->cfg.enemyTexture, val.text, val.lenght);
    }
    key.text = const_cast<char*>("menu_game_over_texture");
    key.lenght = sizeof("menu_game_over_texture")-1;
    index = symbols.Find(key);
    if(index != ~u32(0)) {
        Token val = symbols.array[index].value;
        state->cfg.gameOverTexture = (char*)state->linAllocator.Allocate(val.lenght+1);
        state->cfg.gameOverTexture[val.lenght] = 0;
        MemCpy(state->cfg.gameOverTexture, val.text, val.lenght);
    }
    key.text = const_cast<char*>("menu_bgk_texture");
    key.lenght = sizeof("menu_bgk_texture")-1;
    index = symbols.Find(key);
    if(index != ~u32(0)) {
        Token val = symbols.array[index].value;
        state->cfg.menuTexture = (char*)state->linAllocator.Allocate(val.lenght+1);
        state->cfg.menuTexture[val.lenght] = 0;
        MemCpy(state->cfg.menuTexture, val.text, val.lenght);
    }
    key.text = const_cast<char*>("menu_win_texture");
    key.lenght = sizeof("menu_win_texture")-1;
    
    index = symbols.Find(key);
    if(index != ~u32(0)) {
        Token val = symbols.array[index].value;
        state->cfg.gameWinTexture = (char*)state->linAllocator.Allocate(val.lenght+1);
        state->cfg.gameWinTexture[val.lenght] = 0;
        MemCpy(state->cfg.gameWinTexture, val.text, val.lenght);
    }

    symbols.Free();
}

GLFWwindow* CreateWindow(u32 HEIGHT, u32 WIDTH, void* user, GLFWwindowsizefun callback) {
    if (!glfwInit()) {
        std::cerr << "Failed to initialize GLFW" << std::endl;
        return nullptr;
    }
    GLFW_CALL(glfwWindowHint(GLFW_RESIZABLE, GLFW_TRUE));
    GLFW_CALL(GLFWwindow* window = glfwCreateWindow(WIDTH, HEIGHT, "Ur invazio v1.0", NULL, NULL));
    if (!window)
    {
        glfwTerminate();
        std::cerr << "Failed to create glfw window" << std::endl;
        return nullptr;
    }
    
    GLFW_CALL(glfwMakeContextCurrent(window));
    GLFW_CALL(glfwSwapInterval(1));
    GLFW_CALL(glfwSetWindowSizeCallback(window, callback));
    GLFW_CALL(glfwSetWindowUserPointer(window, user));

    if (glewInit() != GLEW_OK) {
        glfwTerminate();
        std::cerr << "Failed to initialize glew" << std::endl;
        return nullptr;
    }
    return window;
}
void ShutDown(GameState* state) {
    FREE(state->FrameBuffer);
    FREE(state->Bullets.memoryPointer);
    FREE(state->Enemy.memoryPointer);
    //FREE(state->Buttons.memoryPointer);
    glfwTerminate();
    OS_MEM_RELEASE(base, 64*MEGA_BYTE);
}

void InitBgk(GameState* state) {
    for (i32 k = 0; k < 3; k++) {
        for (i32 i = 0; i < 20; i++) {
            state->layers[k].Stars[i].leftbottomx = rand() % state->WIDTH;
            state->layers[k].Stars[i].leftbottomy = rand() % state->HEIGHT;
            state->layers[k].Stars[i].righttopx = state->layers[k].Stars[i].leftbottomx + rand() % 5 + 10;
            state->layers[k].Stars[i].righttopy = state->layers[k].Stars[i].leftbottomy + rand() % 5 + 10;
            state->layers[k].Stars[i].color = { 255,255,255,0 };
        }
        state->layers[k].Speed = k + 1;
    }
}

KeyBoardState GetKeyBoardState(GameState* state) {
    KeyBoardState ret{};
    GLFW_CALL(ret |= glfwGetKey(state->window, GLFW_KEY_ESCAPE) == GLFW_PRESS ? KEY_ESC : 0);
    GLFW_CALL(ret |= glfwGetKey(state->window, GLFW_KEY_ENTER) == GLFW_PRESS ? KEY_ENTER : 0);
    GLFW_CALL(ret |= glfwGetKey(state->window, GLFW_KEY_SPACE) == GLFW_PRESS ? KEY_SPACE : 0);
    GLFW_CALL(ret |= glfwGetKey(state->window, GLFW_KEY_W) == GLFW_PRESS ? KEY_W : 0);
    GLFW_CALL(ret |= glfwGetKey(state->window, GLFW_KEY_A) == GLFW_PRESS ? KEY_A : 0);
    GLFW_CALL(ret |= glfwGetKey(state->window, GLFW_KEY_S) == GLFW_PRESS ? KEY_S : 0);
    GLFW_CALL(ret |= glfwGetKey(state->window, GLFW_KEY_D) == GLFW_PRESS ? KEY_D : 0);
    GLFW_CALL(ret |= glfwGetMouseButton(state->window , GLFW_MOUSE_BUTTON_1) == GLFW_PRESS ? KEY_LEFT_MOUSE : 0);
    return ret;
}
void WindowResize(GLFWwindow* window, int y, int x) {

    GLFW_CALL(GameState* state = (GameState*)glfwGetWindowUserPointer(window));
    state->HEIGHT = x;
    state->WIDTH = y;

    FREE(state->FrameBuffer);
    state->FrameBuffer = (Pixel*)ALLOCATE(x * y * sizeof(Pixel));
}


i32 main() {

    base = (byte*)OS_MEM_RW_REQUEST(64 * MEGA_BYTE);
    LOG_ASSERT(base, "initial memory request failed");
    init_my_malloc(base, 32*MEGA_BYTE);
    srand(time(0));

    GameState state{};

    state.HEIGHT = 480;
    state.WIDTH = 640;
    state.player.HP = 20;

    state.linAllocator.Init(base+32*MEGA_BYTE, 32*MEGA_BYTE);
    Tokenizer tokenizer;
    u32 size;
    tokenizer.at = (char*)ReadFileTerminated("./res/config.cfg", nullptr, &size);
    LOG_ASSERT(tokenizer.at, "could not open config.cfg");
    tokenizer.line = 1;
    ParseConfigFile(&state, tokenizer);
    FREE(tokenizer.at);
    state.window = CreateWindow(state.HEIGHT, state.WIDTH, &state, WindowResize);
    LOG_ASSERT(state.window, " something went wrong with glfw or glew fix me");

    state.layers = (Layer*)state.linAllocator.Allocate(3 * sizeof(Layer));
    state.FrameBuffer = (Pixel*)ALLOCATE(state.HEIGHT * state.WIDTH * sizeof(Pixel));

    state.Bullets.Init();
    state.Enemy.Init();

    state.player.rect = { Pixel {255, 255, 255, 255}, 100, 100, 250, 250 };
    state.level = 0;
    state.killCount = 0;

    state.currentState = GAME_STATE_MENU;
    
    u32 imgSize;
    state.playerTexture   = LoadTexture(state.cfg.playerTexture,   state.linAllocator.mem, &imgSize);
    state.linAllocator.Allocate(imgSize);
    state.enemyTexture    = LoadTexture(state.cfg.enemyTexture,    state.linAllocator.mem, &imgSize);
    state.linAllocator.Allocate(imgSize);
    state.gameOverTexture = LoadTexture(state.cfg.gameOverTexture, state.linAllocator.mem, &imgSize);
    state.linAllocator.Allocate(imgSize);
    state.menuTexture     = LoadTexture(state.cfg.menuTexture,     state.linAllocator.mem, &imgSize);
    state.linAllocator.Allocate(imgSize);
    state.winTexture      = LoadTexture(state.cfg.gameWinTexture,  state.linAllocator.mem, &imgSize);
    state.linAllocator.Allocate(imgSize);

    LOG_ASSERT(
        state.playerTexture.img     &&
        state.enemyTexture.img      &&
        state.gameOverTexture.img   &&
        state.menuTexture.img       &&
        state.winTexture.img, "textures could not be opened"
    );
    
    state.shouldRun = true;
    while(state.shouldRun) {

        GLFW_CALL(glfwPollEvents());
        switch(state.currentState) {
        case GAME_STATE_MENU:
            {
                DrawMenuState(&state);
                if(state.keys & KEY_SPACE) state.currentState = GAME_STATE_PLAYING;
                break;
            }
        case GAME_STATE_PLAYING:
            Input(&state);
            Update(&state);
            DrawPlayinState(&state);
            break;
        case GAME_STATE_WON:
            DrawTextureRectTransparent(&state, Rectangle{ {255,255,255,255}, 0,0, (i32)state.WIDTH,(i32)state.HEIGHT }, &state.winTexture, false, false);
            break;
        case GAME_STATE_GAME_OVER:
            DrawTextureRectTransparent(&state, Rectangle{ {255,255,255,255}, 0,0, (i32)state.WIDTH,(i32)state.HEIGHT }, &state.gameOverTexture, false, false);
            break;
        default:LOG_ASSERT(false, "illegal game state");
        }

        //Clear(&state);
        //DrawTextureRect(&state, Rectangle{ {255,255,255,255}, 0,0, (i32)state.WIDTH,(i32)state.HEIGHT }, &state.playerTexture);

        GLFW_CALL(glDrawPixels(state.WIDTH, state.HEIGHT, GL_RGBA, GL_UNSIGNED_BYTE, state.FrameBuffer));
        GLFW_CALL(glfwSwapBuffers(state.window));
        state.keys = GetKeyBoardState(&state);
        GLFW_CALL(state.shouldRun &= !((state.keys & KEY_ESC) || glfwWindowShouldClose(state.window)));
        state.tick++;
    }

    /*
    $(OBJS): $(CPP_FILES)
	$(COMPILER) $(CPP_FILES) -I $(INCLUDE_PATH) $(COMPILER_FLAGS) $(O_FILES)

    build: folder $(OBJS)
        $(COMPILER) $(OBJS) $(A_FILES) -o $(OUTPUT_PATH)/$(OUTPUT_PATH)

    run: build
        $(OUTPUT_PATH)/$(OUTPUT_BINARY_NAME)

    clean:
        rm $(O_FILES)
    */

    ShutDown(&state);
}