//
//  main.m
//  debug-objc
//
//  Created by Closure on 2018/12/4.
//

#import <Foundation/Foundation.h>

#import "TestObj.h"

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // insert code here...
        
        TestObj *obj = [[TestObj alloc] init];
        
        __weak weakObj = obj;
        
        [obj setBlock:^(id  _Nonnull obj) {
            NSLog(@"%@",weakObj);
        }];
        
    }
    return 0;
}
