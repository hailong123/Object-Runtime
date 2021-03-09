/*
 * Copyright (c) 2010-2011 Apple Inc. All rights reserved.
 *
 * @APPLE_LICENSE_HEADER_START@
 * 
 * This file contains Original Code and/or Modifications of Original Code
 * as defined in and that are subject to the Apple Public Source License
 * Version 2.0 (the 'License'). You may not use this file except in
 * compliance with the License. Please obtain a copy of the License at
 * http://www.opensource.apple.com/apsl/ and read it before using this
 * file.
 * 
 * The Original Code and all software distributed under the License are
 * distributed on an 'AS IS' basis, WITHOUT WARRANTY OF ANY KIND, EITHER
 * EXPRESS OR IMPLIED, AND APPLE HEREBY DISCLAIMS ALL SUCH WARRANTIES,
 * INCLUDING WITHOUT LIMITATION, ANY WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE, QUIET ENJOYMENT OR NON-INFRINGEMENT.
 * Please see the License for the specific language governing rights and
 * limitations under the License.
 * 
 * @APPLE_LICENSE_HEADER_END@
 */

#ifndef _OBJC_WEAK_H_
#define _OBJC_WEAK_H_

#include <objc/objc.h>
#include "objc-config.h"

__BEGIN_DECLS

/*
The weak table is a hash table governed by a single spin lock.
An allocated blob of memory, most often an object, but under GC any such 
allocation, may have its address stored in a __weak marked storage location 
through use of compiler generated write-barriers or hand coded uses of the 
register weak primitive. Associated with the registration can be a callback 
block for the case when one of the allocated chunks of memory is reclaimed. 
The table is hashed on the address of the allocated memory.  When __weak 
marked memory changes its reference, we count on the fact that we can still 
see its previous reference.

So, in the hash table, indexed by the weakly referenced item, is a list of 
all locations where this address is currently being stored.
 
For ARC, we also keep track of whether an arbitrary object is being 
deallocated by briefly placing it in the table just prior to invoking 
dealloc, and removing it via objc_clear_deallocating just prior to memory 
reclamation.

*/
//用于 伪装 __weak 变量的地址 即用于伪装 objc_object * 的地址
// The address of a __weak variable.
// These pointers are stored disguised so memory analysis tools
// don't see lots of interior pointers from the weak table into objects.
//__weak 变量的地址(objc_object **)这些指针是伪装存储的, 因此内存分析工具不会看到从 weak table 到 objects 的大量的内部指针

//这里的 T 是 objc_object * 那么 DisguiseedPtr 里面的 T* 就是 objc_object** 即为指针的指针
typedef DisguisedPtr<objc_object *> weak_referrer_t;

#if __LP64__
#define PTR_MINUS_2 62
#else
#define PTR_MINUS_2 30
#endif

/**
 * The internal structure stored in the weak references table. 
 * It maintains and stores
 * a hash set of weak references pointing to an object.
 * If out_of_line_ness != REFERRERS_OUT_OF_LINE then the set
 * is instead a small inline array.
 */

/*
    内部结构存储在弱引用表中, 它维护和存储指向对象的一组弱引用的哈希(weak_referrer_t). 如果
 out_of_line_ness != REFERRERS_OUT_OF_LINE (0b10), 则该集合为小型内联数组,(长度为4的
 weak_referrer_t 数组)
 */
#define WEAK_INLINE_COUNT 4

// out_of_line_ness field overlaps with the low two bits of inline_referrers[1].
// inline_referrers[1] is a DisguisedPtr of a pointer-aligned address.
// The low two bits of a pointer-aligned DisguisedPtr will always be 0b00
// (disguised nil or 0x80..00) or 0b11 (any other address).
// Therefore out_of_line_ness == 0b10 is used to mark the out-of-line state.

/*
    out_of_line_ness 字段与 inline_referrers[1] 的低两位内存空间重叠. inline_referrers[1]
 是指针对齐地址的 DisguisedPtr. 指针对齐的 DisguisedPtr 的低两位始终为 0b00 (8字节对齐取得的地址
 的二进制表示的后两位始终是0) (伪装为 nil 或 0x80..000) 或 0b11 (任何其他的地址) 因此. out_of_line_ness
 == 0b10 可用于标记 out_of_line状态, 即 struct weak_entry_t 内部是使用哈希表存储 weak_referrer_t
 而不再使用那个长度为 4 的weak_referrer_t 数组
 */
#define REFERRERS_OUT_OF_LINE 2

//存放某个对象的所有弱引用指针 如果弱引用对象数量不超过4个就保存在结构体数组中
//inline_referrers(数组) 否则保存在referrers(哈希表)中.
struct weak_entry_t {
    DisguisedPtr<objc_object> referent; //弱引用对象
    union {
        struct {
            weak_referrer_t *referrers; //弱引用数组 (内部存放的时候 指针的地址 即 指针的指针)
            uintptr_t        out_of_line_ness : 2;//(0x10)
            uintptr_t        num_refs : PTR_MINUS_2;  //引用数量 针对不同的平台 num_refs 是 高 62bit 或者是 高 30 bit
            uintptr_t        mask;
            // 可能会发生 hash 冲突的最大次数，用于判断是否出现了逻辑错误，（hash 表中的冲突次数绝对不会超过该值）
            // 该值在新建 weak_entry_t 和插入新的 weak_referrer_t 时会被更新，它一直记录的都是最大偏移值
            uintptr_t        max_hash_displacement; //最大哈希冲突值
        };
        //弱引用数组 weak_referrer_t 小于等于 4 用这个
        struct {
            // out_of_line_ness field is low bits of inline_referrers[1]
            weak_referrer_t  inline_referrers[WEAK_INLINE_COUNT];
        };
    };

    //判断是否使用 referrers 来存储弱引用指针
    bool out_of_line() {
        return (out_of_line_ness == REFERRERS_OUT_OF_LINE);
    }

    //memcpy 复制内存指针方法
    //覆盖老数据
    weak_entry_t& operator=(const weak_entry_t& other) {
        memcpy(this, &other, sizeof(other));
        return *this;
    }

    // 构造方法
    // newReferent 是原始对象的指针
    // newReferenr 是指向 newReferrer的弱引用变量的指针
    
    // 初始化列表 referent(newReferent) 会调用: DisguisedPtr(T* ptr) : value(disguise(ptr)) { } 构造函数，
    // 调用 disguise 函数把 newReferent 转化为一个整数赋值给 value
    weak_entry_t(objc_object *newReferent, objc_object **newReferrer)
        : referent(newReferent)
    {
        //把 newReferrer 放在数组 0 位, 也会调用 DisguisedPtr 构造函数, 把 newReferrer 转化为整数保存
        inline_referrers[0] = newReferrer;
        //循环把 inline_referrers 数组的剩余3位都置为nil
        for (int i = 1; i < WEAK_INLINE_COUNT; i++) {
            inline_referrers[i] = nil;
        }
    }
};

/**
 * The global weak references table. Stores object ids as keys,
 * and weak_entry_t structs as their values.
 */
/*
    全局保存弱引用的哈希表, 以 object ids 为 keys 以 weak_entry_t 为 values
 */
struct weak_table_t {
    weak_entry_t *weak_entries;//存储 weak_entry_t的哈希数组
    size_t    num_entries; //当前 weak_entries 内保存的 weak_entry_t 的数量, 哈希数组内保存的元素个数
    uintptr_t mask; //哈希数组 的总长度 -1 会参与 hash 值计算
    
    /*
        记录所有项的最大偏移量 即发生 hash 冲突的最大次数
     因为会有 hash 冲突 而 weak_table_t 采用了开放地址寻址来解决
     所以某个weak_entry_t 实际存储的位置并不一定是 hash 函数计算出来的位置
     */
    uintptr_t max_hash_displacement;
};

/// Adds an (object, weak pointer) pair to the weak table.
id weak_register_no_lock(weak_table_t *weak_table, id referent, 
                         id *referrer, bool crashIfDeallocating);

/// Removes an (object, weak pointer) pair from the weak table.
void weak_unregister_no_lock(weak_table_t *weak_table, id referent, id *referrer);

#if DEBUG
/// Returns true if an object is weakly referenced somewhere.
bool weak_is_registered_no_lock(weak_table_t *weak_table, id referent);
#endif

/// Called on object destruction. Sets all remaining weak pointers to nil.
void weak_clear_no_lock(weak_table_t *weak_table, id referent);

__END_DECLS

#endif /* _OBJC_WEAK_H_ */
