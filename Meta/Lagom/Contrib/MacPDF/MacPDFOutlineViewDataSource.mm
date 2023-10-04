/*
 * Copyright (c) 2023, Nico Weber <thakis@chromium.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#import "MacPDFOutlineViewDataSource.h"

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

    // XXX do something if root has 0 children
    if (!item) {
        return _outline->children.size();
    }

    auto const* outline_item = (OutlineItemWrapper*)item;
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
