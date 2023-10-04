/*
 * Copyright (c) 2023, Nico Weber <thakis@chromium.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#import "MacPDFOutlineViewDataSource.h"

// Objective-C wrapper of PDF::OutlineItem, to launder it through the NSOutlineViewDataSource protocol.
@interface OutlineItemWrapper : NSObject
{
    // NonnullRefPtr really, but Objective-C objects cannot be initialized with that.
@public
    RefPtr<PDF::OutlineItem> _item;
}

- (instancetype)initWithItem:(NonnullRefPtr<PDF::OutlineItem>)item;
@end

@implementation OutlineItemWrapper
- (instancetype)initWithItem:(NonnullRefPtr<PDF::OutlineItem>)item
{
    if (self = [super init]; !self)
        return nil;
    _item = move(item);
    return self;
}
@end

@interface MacPDFOutlineViewDataSource ()
{
    RefPtr<PDF::OutlineDict> _outline;
}
@end

@implementation MacPDFOutlineViewDataSource

- (instancetype)initWithOutline:(RefPtr<PDF::OutlineDict>)outline
{
    if (self = [super init]; !self)
        return nil;
    _outline = move(outline);
dbgln("init {}", _outline);
    return self;
}

- (BOOL)hasOutline
{
    return _outline && !_outline->children.is_empty();
}

#pragma mark - NSOutlineViewDataSource

- (id)outlineView:(NSOutlineView *)outlineView child:(NSInteger)index ofItem:(nullable id)item
{
    if (![self hasOutline])
        return nil;

dbgln("child: {} ofItem:{}", index, item);
    if (!item)
        return [[OutlineItemWrapper alloc] initWithItem:_outline->children[index]];

    auto const* outline_item = (OutlineItemWrapper*)item;
    return [[OutlineItemWrapper alloc] initWithItem:outline_item->_item->children[index]];
}

- (BOOL)outlineView:(NSOutlineView *)outlineView isItemExpandable:(id)item
{
    return [self outlineView:outlineView numberOfChildrenOfItem:item] > 0;
}

- (NSInteger)outlineView:(NSOutlineView *)outlineView numberOfChildrenOfItem:(nullable id)item
{
    if (![self hasOutline])
        return 1;

dbgln("hi? {} {}", item, _outline);
    // XXX do something if root has 0 children
    if (!item) {
dbgln("numberOfChildrenOfItem root {}", _outline->children.size());
        return _outline->children.size();
    }

    auto const* outline_item = (OutlineItemWrapper*)item;
dbgln("numberOfChildrenOfItem root {}", outline_item->_item->children.size());
    return outline_item->_item->children.size();
}

- (id)outlineView:(NSOutlineView *)outlineView objectValueForTableColumn:(nullable NSTableColumn *)tableColumn byItem:(nullable id)item
{
    if (![self hasOutline])
            return @"(no outline)";  // FIXME: Maybe put filename here instead?

    auto const* outline_item = (OutlineItemWrapper*)item;
    return [NSString stringWithFormat:@"%s", outline_item->_item->title.characters()];  // XXX encoding?
}

@end
