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

#include "objc-private.h"

#include "objc-weak.h"

#include <stdint.h>
#include <stdbool.h>
#include <sys/types.h>
#include <libkern/OSAtomic.h>

//用于获取 weak_entry_t 或 weak_table_t 的哈希数组当前分配的总容量
#define TABLE_SIZE(entry) (entry->mask ? entry->mask + 1 : 0)

//向指定 weak_entry_t 里面添加 new_referrer(weak 变量的地址)
static void append_referrer(weak_entry_t *entry, objc_object **new_referrer);

BREAKPOINT_FUNCTION(
    void objc_weak_error(void)
);

static void bad_weak_table(weak_entry_t *entries)
{
    //_objc_fatal 用来退出程序或者中止运行并打印原因
    _objc_fatal("bad weak table at %p. This may be a runtime bug or a "
                "memory error somewhere else.", entries);
}

/** 
 * Unique hash function for object pointers only.
 * 
 * @param key The object pointer
 * 
 * @return Size unrestricted hash of pointer.
 */
//唯一的哈希函数仅适用于对象指针
//对一个objc_object 对象的指针进行求哈希值 用于从 weak_table_t 哈希表中取得对象对应的
//weak_entry_t
static inline uintptr_t hash_pointer(objc_object *key) {
    return ptr_hash((uintptr_t)key);
}

/** 
 * Unique hash function for weak object pointers only.
 * 
 * @param key The weak object pointer. 
 * 
 * @return Size unrestricted hash of pointer.
 */
//唯一的哈希函数仅适用于对象指针
//对一个 objc_object 对象的指针的指针(此处指 weak 变量的地址) 求哈希值 用于从 weak_entry_t
//哈希表中取得 weak_referrer_t 把其保存的弱引用变量指向置为nil或者从哈希表中移除
static inline uintptr_t w_hash_pointer(objc_object **key) {
    //把指针强转为 unsigned long 然后调用 ptr_hash 函数
    return ptr_hash((uintptr_t)key);
}

/** 
 * Grow the entry's hash table of referrers. Rehashes each
 * of the referrers.
 * 
 * @param entry Weak pointer hash set for a particular object.
 */
//对 weak_entry_t 的哈希数组进行扩容 并插入一个新的 new_referrer 原有数据重新哈希化放在新空间内
__attribute__((noinline, used))
static void grow_refs_and_insert(weak_entry_t *entry, 
                                 objc_object **new_referrer)
{
    //DEBUG 下的断言 确保当前 weak_entry_t 使用的是 hash 数组模式
    ASSERT(entry->out_of_line());

    //新容量为旧容量的2倍
    size_t old_size = TABLE_SIZE(entry);
    size_t new_size = old_size ? old_size * 2 : 8;

    //记录当前已使用的容量
    size_t num_refs = entry->num_refs;
    //记录旧哈希数组起始地址, 在最后要进行释放
    weak_referrer_t *old_refs = entry->referrers;
    //mask 依然是总容量减 1
    entry->mask = new_size - 1;
    
    //为新 hash 数组申请空间
    //长度为: 总容量*sizeof(weak_referrer_t)(8)个字节
    entry->referrers = (weak_referrer_t *)
        calloc(TABLE_SIZE(entry), sizeof(weak_referrer_t));
    //默认为0
    entry->num_refs = 0;
    entry->max_hash_displacement = 0;
    
    for (size_t i = 0; i < old_size && num_refs > 0; i++) {
        if (old_refs[i] != nil) {
            //把旧哈希数组里的数据都放进新的哈希数组内
            append_referrer(entry, old_refs[i]);
            //旧的哈希数组的长度自减
            num_refs--;
        }
    }
    // Insert
    //然后把入参传入的 new_referrer 插入新的哈希数组, 前面的铺垫都是在做 '数据转移'
    append_referrer(entry, new_referrer);
    //把旧哈希数据释放
    if (old_refs) free(old_refs);
}

/** 
 * Add the given referrer to set of weak pointers in this entry.
 * 添加给定的 referrer 到weak_entry_t 的哈希数组(或给定长为 4 的内部数组)
 * Does not perform duplicate checking (b/c weak pointers are never
 * added to a set twice). 
 * 不执行重复检查, weak 指针永远不会添加两次
 *
 * @param entry The entry holding the set of weak pointers. 
 * @param new_referrer The new weak pointer to be added.
 */

static void append_referrer(weak_entry_t *entry, objc_object **new_referrer)
{
    //如果使用的是 inline_referrers 找到一个空位就插入
    if (! entry->out_of_line()) {
        // Try to insert inline.
        for (size_t i = 0; i < WEAK_INLINE_COUNT; i++) {
            if (entry->inline_referrers[i] == nil) {
                //如果找到空的位置就插入
                entry->inline_referrers[i] = new_referrer;
                return;
            }
        }

        // Couldn't insert inline. Allocate out of line.
        //如果 inline_referrers 没有空位了 那么就需要更换使用 共用体 中的结构体类型
        
        //使用 weak_referrer_t *referrers 申请空间
        weak_referrer_t *new_referrers = (weak_referrer_t *)
            calloc(WEAK_INLINE_COUNT, sizeof(weak_referrer_t));
        // This constructed table is invalid, but grow_refs_and_insert
        // will fix it and rehash it.
        //将 inline_referrers 中的数据复制到 referrers 中 "数据转移"
        for (size_t i = 0; i < WEAK_INLINE_COUNT; i++) {
            new_referrers[i] = entry->inline_referrers[i];
        }
        
        //给 referrers 赋值
        entry->referrers = new_referrers;
        //表示目前弱引用为 4
        entry->num_refs = WEAK_INLINE_COUNT;
        
        //标识 referrers 启用 对应out_of_line() 方法的调用
        //标记 weak_entry_t 开始使用 哈希数组保存弱引用的指针
        entry->out_of_line_ness = REFERRERS_OUT_OF_LINE;
        //更新mask值 总为总数量 -1
        entry->mask = WEAK_INLINE_COUNT-1;
        //此时哈希冲突偏移值为0
        entry->max_hash_displacement = 0;
    }

    //对于动态数组的扩容处理
    ASSERT(entry->out_of_line());

    //判断是否需要扩容
    if (entry->num_refs >= TABLE_SIZE(entry) * 3/4) {
        //weak_entry_t 哈希数组扩容并插入 new_referrer
        return grow_refs_and_insert(entry, new_referrer);
    }
    
    //不需要扩容这正常插入
    size_t begin = w_hash_pointer(new_referrer) & (entry->mask);
    size_t index = begin;
    size_t hash_displacement = 0;
    
    //寻找空位置 进行插入
    while (entry->referrers[index] != nil) {
        hash_displacement++;
        index = (index+1) & entry->mask;
        //在 index == begin 之前一定能找到空位置 因为前面已经已经有一个 超过 3/4 占用后的扩容机制
        if (index == begin) bad_weak_table(entry);
    }
    
    //这里是更新当前 entry 存储了多少对象的数
    if (hash_displacement > entry->max_hash_displacement) {
        entry->max_hash_displacement = hash_displacement;
    }
    
    //在空位置上插入需要保存的对象
    weak_referrer_t &ref = entry->referrers[index];
    ref = new_referrer;
    //更新 entry 的弱引用数
    entry->num_refs++;
}

/** 
 * Remove old_referrer from set of referrers, if it's present.
 * Does not remove duplicates, because duplicates should not exist. 
 * 
 * @todo this is slow if old_referrer is not present. Is this ever the case? 
 *
 * @param entry The entry holding the referrers.
 * @param old_referrer The referrer to remove. 
 */
static void remove_referrer(weak_entry_t *entry, objc_object **old_referrer)
{
    //如果使用的是 inline_referrers(定长为 4 的数组) 就在这里找 并置为nil
    if (! entry->out_of_line()) {
        for (size_t i = 0; i < WEAK_INLINE_COUNT; i++) {
            //循环找到 old_referrer的位置, 把它的位置清空nil. 表示把old_referrer从数组中移除了
            if (entry->inline_referrers[i] == old_referrer) {
                //找到后置为nil
                entry->inline_referrers[i] = nil;
                return;
            }
        }
        
        //如果当前 weak_entry_t 不包含传入的 old_referrer 则明显发生了错误 执行objc_weak_error 函数
        _objc_inform("Attempted to unregister unknown __weak variable "
                     "at %p. This is probably incorrect use of "
                     "objc_storeWeak() and objc_loadWeak(). "
                     "Break on objc_weak_error to debug.\n", 
                     old_referrer);
        objc_weak_error();
        return;
    }
    
    //如果使用的是 referrers 就来到这里 (数量大于4)
    //查找的方式一样, 只是数据源变为 referrers
    size_t begin = w_hash_pointer(old_referrer) & (entry->mask);
    size_t index = begin;
    size_t hash_displacement = 0;
    while (entry->referrers[index] != old_referrer) {
        index = (index+1) & entry->mask;
        if (index == begin) bad_weak_table(entry);
        hash_displacement++;
        if (hash_displacement > entry->max_hash_displacement) {
            _objc_inform("Attempted to unregister unknown __weak variable "
                         "at %p. This is probably incorrect use of "
                         "objc_storeWeak() and objc_loadWeak(). "
                         "Break on objc_weak_error to debug.\n", 
                         old_referrer);
            objc_weak_error();
            return;
        }
    }
    
    //删除弱引用对象
    entry->referrers[index] = nil;
    
    //引用计数 -1
    entry->num_refs--;
}

/** 
 * Add new_entry to the object's table of weak references. 在对象的弱引用表中添加 new_entry
 * Does not check whether the referent is already in the table. 不检查 referent 是否已存在表中
 */
//方法中不判断全局弱引用表中是否存在当前对象直接插入
static void weak_entry_insert(weak_table_t *weak_table, weak_entry_t *new_entry)
{
    //哈希数组的地址
    weak_entry_t *weak_entries = weak_table->weak_entries;
    ASSERT(weak_entries != nil);

    //调用hash 函数找到 new_entry 在 weak_table_t 的哈希数组中的位置. 可能会发生 hash 冲突 & mask 的原理同上
    size_t begin = hash_pointer(new_entry->referent) & (weak_table->mask);
    size_t index = begin;
    size_t hash_displacement = 0;
    
    //在全局弱引用表中哈希查找空位
    while (weak_entries[index].referent != nil) {
        //如果发生哈希冲突 +1 继续向下探测
        index = (index+1) & weak_table->mask;
        //如果index 每次加 1 加到的值等于 begin 还是没有找到空位置 则触发 bad_weak_table 致命错误
        if (index == begin) bad_weak_table(weak_entries);
        //记录偏移值 用于更新 max_hash_displacement
        hash_displacement++;
    }

    //直接把传入的弱引用对象插入全局弱引用表中
    weak_entries[index] = *new_entry;
    //全局弱引用表中弱引用对象计数 + 1
    weak_table->num_entries++;

    //更新最大哈希冲突数
    if (hash_displacement > weak_table->max_hash_displacement) {
        weak_table->max_hash_displacement = hash_displacement;
    }
}

//扩大和缩小空间都会调用这个公共函数
static void weak_resize(weak_table_t *weak_table, size_t new_size)
{
    //获取到当前哈希数组的长度
    size_t old_size = TABLE_SIZE(weak_table);

    //取得旧的 weak_entries 哈希数组的起始地址
    weak_entry_t *old_entries = weak_table->weak_entries;
    
    //为新的 weak_entires 哈希数组申请指定长度的空间 并把起始地址返回
    weak_entry_t *new_entries = (weak_entry_t *)
        calloc(new_size, sizeof(weak_entry_t));

    //更新mask 仍是总长度 -1
    weak_table->mask = new_size - 1;
    //更新hash 数组起始地址
    weak_table->weak_entries = new_entries;
    //最大哈希冲突偏移值 默认为0
    weak_table->max_hash_displacement = 0;
    
    //当前哈希数组的占用数量 默认为0
    weak_table->num_entries = 0;  // restored by weak_entry_insert below
    
    //循环插入 如果有旧的 weak_entry_t 需要放到新的空间内
    if (old_entries) {
        weak_entry_t *entry;
        //旧哈希数组的末尾
        weak_entry_t *end = old_entries + old_size;
        //循环调用 weak_entry_insert 把旧哈希数组中的 weak_entry_t 插入到新的哈希数组中
        for (entry = old_entries; entry < end; entry++) {
            if (entry->referent) {
                weak_entry_insert(weak_table, entry);
            }
        }
        
        //释放旧表
        free(old_entries);
    }
}

// Grow the given zone's table of weak references if it is full.
//如果大于等于原始大小的3/4就需要扩容, 如果原始表不存在, 就生成新表大小 为 64字节
static void weak_grow_maybe(weak_table_t *weak_table)
{
    size_t old_size = TABLE_SIZE(weak_table);

    // Grow if at least 3/4 full.
    if (weak_table->num_entries >= old_size * 3 / 4) {
        //如果 old_size = 0 就扩容为64 如果不是, 则扩容之前的两倍
        weak_resize(weak_table, old_size ? old_size*2 : 64);
    }
}

// Shrink the table if it is mostly empty. 对表进行收缩
//原始全局散列表 old_size > 1024 并且全局散列表下的弱引用对象的个数 num_entries < old_size / 16 才进行收缩
static void weak_compact_maybe(weak_table_t *weak_table)
{
    size_t old_size = TABLE_SIZE(weak_table);

    // Shrink if larger than 1024 buckets and at most 1/16 full.
    if (old_size >= 1024  && old_size / 16 >= weak_table->num_entries) {
        //收缩到原来表的一半
        weak_resize(weak_table, old_size / 8);
        // leaves new table no more than 1/2 full
    }
}


/**
 * Remove entry from the zone's table of weak references.
 */
//从全局弱引用表中删除这个弱引用对象的数据 清空所占的内存空间
static void weak_entry_remove(weak_table_t *weak_table, weak_entry_t *entry)
{
    // remove entry
    //如果使用的是 referrers 释放 referrers
    if (entry->out_of_line()) free(entry->referrers);
    
    //以 entry为起始地址的前sizeof(*entry)个字节区域清零
    bzero(entry, sizeof(*entry));

    //全局弱引用表 弱引用对象数量-1
    weak_table->num_entries--;
    
    //收缩表大小, 不浪费空间
    weak_compact_maybe(weak_table);
}


/** 
 * Return the weak reference table entry for the given referent.
 * If there is no entry for referent, return NULL.
 * Performs a lookup.
 *
 * @param weak_table 通过 &SideTables()[referent] 可从全局的SideTable 中找到 referent 所处的 SideTable->weak_table_t
 * @param referent The object. Must not be nil. 返回值是 weak_entry_t 指针, weak_entry_t 中保存了 referent 的所有弱引用变量的地址
 * 
 * @return The table of weak referrers to this object. 
 */
//根据给定的 referent (我们的对象变量)和weak_table_t哈希表, 查找其中的 weak_entry_t(存放所有
//指向 referent 的弱引用变量的地址哈希表)并返回 如果没有找到就返回NULL
static weak_entry_t *
weak_entry_for_referent(weak_table_t *weak_table, objc_object *referent)
{
    ASSERT(referent);

    //weak_table_t 中哈希数组的入口
    weak_entry_t *weak_entries = weak_table->weak_entries;

    //如果为空 则返回nil
    if (!weak_entries) return nil;

    //hash_pointer 哈希函数返回值 与 mask 做与操作 防止 index 越界 这里的 & mask 操作很巧妙(如同取模操作)
    size_t begin = hash_pointer(referent) & weak_table->mask;
    
    size_t index = begin;
    size_t hash_displacement = 0;
    
    //进行 哈希查找
    //直到在全局弱引用表里面找到 referrer 或者 大于最大哈希查找冲突数 返回
    while (weak_table->weak_entries[index].referent != referent) {
        //如果发生了 哈希冲突 +1 继续往下探测(开放地址寻址方式)
        index = (index+1) & weak_table->mask;
        //如果 index 每次 +1 加到的值等于 begin 还没有找到 weak_entry_t 则触发 bad_weak_table 致命错误
        if (index == begin) bad_weak_table(weak_table->weak_entries);
        //记录探测偏移了多远
        hash_displacement++;
        //如果大于最大哈希查找冲突数, 就代表没有找到了
        if (hash_displacement > weak_table->max_hash_displacement) {
            return nil;
        }
    }
    
    //返回找到的对象
    return &weak_table->weak_entries[index];
}

/** 
 * Unregister an already-registered weak reference.
 * This is used when referrer's storage is about to go away, but referent
 * isn't dead yet. (Otherwise, zeroing referrer later would be a
 * bad memory access.)
 * Does nothing if referent/referrer is not a currently active weak reference.
 * Does not zero referrer.
 * 
 * FIXME currently requires old referent value to be passed in (lame)
 * FIXME unregistration should be automatic if referrer is collected
 * 
 * @param weak_table The global weak table.  全局弱引用表
 * @param referent The object. //引用对象
 * @param referrer The weak reference. //弱引用
 */
/*
    取消注册一个已经注册的弱引用
    当referrer的存储即将消失, 但referrer还没有死时使用,(否则, 稍后将referrer归零将是一个错误的内存访问)
    如果 referrer/referent 不是当前激活的弱引用, 则不作任何操作
    不为0的引用
    FIXME 目前需要旧的值参考值被传入(lame)
    如果 referrer被收集, FIXME将自动取消注册
 */
/*
    注销以前注册的弱引用. 该方法用于referrer 的存储即将消失, 但是 referent 还正常存在.
 (否则, referrer被释放后, 可能会造成一个错误的内存访问, 即对象还没有释放. 但是weak变量
 已经释放了. 这时候再去访问weak变量会导致野指针访问) 如果 referent/referrer 不是当前
 有效的弱引用,则不执行任何操作
 */
void
weak_unregister_no_lock(weak_table_t *weak_table, id referent_id, 
                        id *referrer_id)
{
    //id 转化为 objc_object * 对象的指针
    objc_object *referent = (objc_object *)referent_id;
    //referrer_id 是指向 weak 变量的地址 所以这里是 **
    objc_object **referrer = (objc_object **)referrer_id;

    weak_entry_t *entry;

    //如果值为空 直接返回
    if (!referent) return;

    //开始查找

    //从weak_table 中找到 referent 的 weak_entry_t
    if ((entry = weak_entry_for_referent(weak_table, referent))) {
        //从弱引用表中删除
        remove_referrer(entry, referrer);
        
        //用于判断entry内容是否是空的
        bool empty = true;
        
        //如果使用了 weak_referrer_t *referrer 且 引用数 num_refs != 0 代表还有对象在用
        if (entry->out_of_line()  &&  entry->num_refs != 0) {
            empty = false;
        }
        else {
            //否则 使用了 inline_referrers 判断 inline_referrers有没有值
            for (size_t i = 0; i < WEAK_INLINE_COUNT; i++) {
                if (entry->inline_referrers[i]) {
                    empty = false; 
                    break;
                }
            }
        }

        //如果entry 中的弱引用地址都已经清空了. 则连带也删除这个 entry 类似数组已经空了 则把数组也删了
        if (empty) {
            //如果为空 则移除这 entry
            weak_entry_remove(weak_table, entry);
        }
    }

    // Do not set *referrer = nil. objc_storeWeak() requires that the 
    // value not change.
}

/** 
 * Registers a new (object, weak pointer) pair. Creates a new weak 注册一个新的 object 和 weak pointer 的配对
 * object entry if it does not exist. 创建一个新的弱对象条目(如果它不存在)
 * 
 * @param weak_table The global weak table. 全局的弱引用表
 * @param referent The object pointed to by the weak reference. 弱引用对象
 * @param referrer The weak pointer address. 弱引用对象的地址
 */
/*
    添加一对 (object, weak pointer) 到弱引用表里, (即当一个对象存在第一个指向它的weak变量时, 此时会把注册进
 weak_table_t的哈希表中,同时也会把这第一个weak变量的地址保存进对象的weak_entry_t哈希表中,如果这个weak变量不是第一个
 的话,表明这个对象此时已经存在于weak_table_t哈希表中了. 此时只需要把这个指向他的weak变量的地址保存进该对象的weak_entry_t
 哈希表中)
 */
id 
weak_register_no_lock(weak_table_t *weak_table, id referent_id, 
                      id *referrer_id, bool crashIfDeallocating)
{
    //准备插入的对象
    objc_object *referent = (objc_object *)referent_id;
    //插入对象的地址
    objc_object **referrer = (objc_object **)referrer_id;

    //如果是 isTaggedPointer 对象或者为 nil 直接返回
    if (!referent  ||  referent->isTaggedPointer()) return referent_id;

    // ensure that the referenced object is viable
    bool deallocating;
    
    //判断ISA是否有自定义的释放方法
    //此比特位会在该类或者父类重写下列方法是返回true
    //retain/release/autorelease/retainCount/_tryRetain/_isDeallocating/retainWeakReference/allowsWeakReference返回true

    if (!referent->ISA()->hasCustomRR()) {
        deallocating = referent->rootIsDeallocating();
    }
    else {
        //使用 lookupimportforward.
        //这样我们就可以避免class_getInstanceMethod中的assert
        //因为我们故意在锁被持有的情况下调用这个callout
        BOOL (*allowsWeakReference)(objc_object *, SEL) = 
            (BOOL(*)(objc_object *, SEL))
            object_getMethodImplementation((id)referent, 
                                           @selector(allowsWeakReference));
        if ((IMP)allowsWeakReference == _objc_msgForward) {
            return nil;
        }
        deallocating =
            ! (*allowsWeakReference)(referent, @selector(allowsWeakReference));
    }

    //如果已经释放或者对象不能进行weak引用 且 crashIfDeallocating 为 true 则崩溃
    if (deallocating) {
        if (crashIfDeallocating) {
            _objc_fatal("Cannot form weak reference to instance (%p) of "
                        "class %s. It is possible that this object was "
                        "over-released, or is in the process of deallocation.",
                        (void*)referent, object_getClassName((id)referent));
        } else {
            return nil;
        }
    }

    // now remember it and where it is being stored
    //现在记住它和它被存储的位置
    weak_entry_t *entry;
    
    //查找弱引用对象
    if ((entry = weak_entry_for_referent(weak_table, referent))) {
        //查到了就拼接
        append_referrer(entry, referrer);
    } 
    else {
        //如果没有找到 初始化
        weak_entry_t new_entry(referent, referrer);
        //看是否需要扩容 使用 3/4 定律
        weak_grow_maybe(weak_table);
        //将弱引用插入到哈希表中
        weak_entry_insert(weak_table, &new_entry);
    }

    // Do not set *referrer. objc_storeWeak() requires that the 
    // value not change.

    return referent_id;
}


#if DEBUG
//如果一个对象弱引用表的某处 即该对象被保存在弱引用表里(该对象存在弱引用)则返回true
//(已注册 = 存在弱引用, 未注册=不存在弱引用)
bool
weak_is_registered_no_lock(weak_table_t *weak_table, id referent_id) 
{
    return weak_entry_for_referent(weak_table, (objc_object *)referent_id);
}
#endif


/** 
 * Called by dealloc; nils out all weak pointers that point to the 
 * provided object so that they can no longer be used.
 * 
 * @param weak_table 
 * @param referent The object being deallocated. 
 */

//将对象销毁的时候该函数被调用. 设置所有剩余的 __weak变量指向nil.
//调用顺序 dealloc -> clearDeallocating -> sidetable_clearDeallocating() -> weak_clear_no_lock()
void 
weak_clear_no_lock(weak_table_t *weak_table, id referent_id) 
{
    //id 转化为 objc_object * 对象指针
    objc_object *referent = (objc_object *)referent_id;

    //找到 referent 在 weak_table 哈希数组中的 weak_entry_t
    weak_entry_t *entry = weak_entry_for_referent(weak_table, referent);
    
    if (entry == nil) {
        /// XXX shouldn't happen, but does with mismatched CF/objc
        //printf("XXX no entry for clear deallocating %p\n", referent);
        return;
    }
    //取联合体中保存的  referrers 和 count
    // zero out references
    weak_referrer_t *referrers;
    size_t count;
    
    //找出 referent 的弱引用的地址 (weak_referrer_t) 数组以及数组长度
    if (entry->out_of_line()) {
        //哈希数组起始地址
        referrers = entry->referrers;
        //长度 是 mask + 1
        count = TABLE_SIZE(entry);
    } 
    else {
        //记录 inline_referrers的入口
        referrers = entry->inline_referrers;
        //count 是 4
        count = WEAK_INLINE_COUNT;
    }
    
    //循环把 inline_referrers 数组或者 hash 数组中的 weak 变量指向置为 nil
    for (size_t i = 0; i < count; ++i) {
        //weak 变量的指针的指针
        objc_object **referrer = referrers[i];
        if (referrer) {
            //如果 weak 变量指向 referent 则把其指向置为 nil
            if (*referrer == referent) {
                *referrer = nil;
            }
            else if (*referrer) {
                //如果 weak_entry_t 里面存放的weak 变量指向的对象不是 referent
                //可能是错误的调用了 objc_storeWeak 和 objc_loadWeak 函数导致
                //执行 objc_weak_error 进行debug
                _objc_inform("__weak variable at %p holds %p instead of %p. "
                             "This is probably incorrect use of "
                             "objc_storeWeak() and objc_loadWeak(). "
                             "Break on objc_weak_error to debug.\n", 
                             referrer, (void*)*referrer, (void*)referent);
                objc_weak_error();
            }
        }
    }
    
    // 由于 referent 要被释放了，因此 referent 的 weak_entry_t 也要从 weak_table 的哈希数组中移除。确保哈希表的性能以及查找效率。
    weak_entry_remove(weak_table, entry);
}

