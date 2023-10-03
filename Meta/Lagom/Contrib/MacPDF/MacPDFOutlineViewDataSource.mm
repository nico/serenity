/*
 * Copyright (c) 2023, Nico Weber <thakis@chromium.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#import "MacPDFOutlineViewDataSource.h"

@implementation MacPDFOutlineViewDataSource

- (instancetype)init {
    self = [super init];
    if (self) {
        // Initialize your hierarchical data structure, for example, an array of dictionaries
        self.data = [NSMutableArray arrayWithArray:@[
            @{@"name": @"Chapter 1", @"children": @[
                    @{@"name": @"Section 1.1", @"children": @[]},
                    @{@"name": @"Section 1.2", @"children": @[]}
            ]},
            @{@"name": @"Chapter 2", @"children": @[
                    @{@"name": @"Section 2.1", @"children": @[]},
                    @{@"name": @"Section 2.2", @"children": @[]}
            ]}
        ]];
    }
    return self;
}

#pragma mark - NSOutlineViewDataSource

- (id)outlineView:(NSOutlineView *)outlineView child:(NSInteger)index ofItem:(nullable id)item {
    // Return the child item at the specified index of the parent item
    if (!item) {
        return self.data[index];
    }

    if ([item isKindOfClass:[NSDictionary class]]) {
        NSArray *children = item[@"children"];
        if (children && [children isKindOfClass:[NSArray class]] && index < (NSInteger)children.count) {
            return children[index];
        }
    }

    return nil;
}

- (BOOL)outlineView:(NSOutlineView *)outlineView isItemExpandable:(id)item {
    // Check if an item has children that can be expanded
    if ([item isKindOfClass:[NSDictionary class]]) {
        NSArray *children = item[@"children"];
        return children && [children isKindOfClass:[NSArray class]] && children.count > 0;
    }
    return NO;
}

- (NSInteger)outlineView:(NSOutlineView *)outlineView numberOfChildrenOfItem:(nullable id)item {
    // Return the number of children for a given item
    if (!item) {
        return self.data.count;
    }

    if ([item isKindOfClass:[NSDictionary class]]) {
        NSArray *children = item[@"children"];
        if (children && [children isKindOfClass:[NSArray class]]) {
            return children.count;
        }
    }

    return 0;
}

- (id)outlineView:(NSOutlineView *)outlineView objectValueForTableColumn:(nullable NSTableColumn *)tableColumn byItem:(nullable id)item {
    // Return the data to be displayed in the outline view's cells
    if ([item isKindOfClass:[NSDictionary class]]) {
        NSString *name = item[@"name"];
        if (name && [name isKindOfClass:[NSString class]]) {
            return name;
        }
    }
    return nil;
}

@end
