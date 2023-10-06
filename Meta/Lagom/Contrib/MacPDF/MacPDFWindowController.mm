/*
 * Copyright (c) 2023, Nico Weber <thakis@chromium.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#import "MacPDFWindowController.h"

#import "MacPDFDocument.h"
#import "MacPDFOutlineViewDataSource.h"

@interface MacPDFWindowController ()
{
    MacPDFOutlineViewDataSource* _outlineDataSource;
    MacPDFDocument* _pdfDocument;
    IBOutlet MacPDFView* _pdfView;

    NSOutlineView* side_view;
}
@end

@implementation MacPDFWindowController

- (void)dumpConstraints:(NSView*)view
{
    for (NSLayoutConstraint* c in view.constraints)
        NSLog(@"%@", c);

}

- (void)dumpView:(NSView*)view collect:(NSMutableArray*)a
{
    [self dumpConstraints:view];
    [a addObjectsFromArray:view.constraints];

    for (NSView* subview in view.subviews)
        [self dumpView:subview collect:a];
}

- (instancetype)initWithDocument:(MacPDFDocument*)document
{
    auto const style_mask = NSWindowStyleMaskTitled | NSWindowStyleMaskClosable | NSWindowStyleMaskMiniaturizable | NSWindowStyleMaskResizable | NSWindowStyleMaskFullSizeContentView;
    NSWindow* window = [[NSWindow alloc] initWithContentRect:NSMakeRect(0, 0, 600, 800)
                                                   styleMask:style_mask
                                                     backing:NSBackingStoreBuffered
                                                       defer:YES];

    if (self = [super initWithWindow:window]; !self)
        return nil;

    _pdfView = [[MacPDFView alloc] initWithFrame:NSZeroRect];
    [_pdfView setDelegate:self];
    //_pdfView.autoresizingMask = NSViewWidthSizable | NSViewHeightSizable | NSViewMinYMargin | NSViewMaxXMargin;
    //_pdfView.translatesAutoresizingMaskIntoConstraints = NO;
NSLog(@"pdf intrinsic size %@", NSStringFromSize([_pdfView intrinsicContentSize]));

    NSSplitViewController* split_view = [[NSSplitViewController alloc] initWithNibName:nil bundle:nil];
    //split_view.view.translatesAutoresizingMaskIntoConstraints=NO;

    [split_view addSplitViewItem:[self makeSidebarSplitItem]];
    [split_view addSplitViewItem:[NSSplitViewItem splitViewItemWithViewController:[self viewControllerForView:_pdfView]]];

    // Autosave if the sidebar is open or not, and how far.
    // autosaveName only works if identifier is set too.
    // identifier docs: "For programmatically created views, you typically set this value
    // after creating the item but before adding it to a window. [...] For views and controls
    // in a window, the value you specify for this string must be unique on a per-window basis."
    split_view.splitView.autosaveName = @"MacPDFSplitView";
    split_view.splitView.identifier = @"MacPDFSplitViewId";

    window.contentViewController = split_view;

    NSToolbar* toolbar = [[NSToolbar alloc] initWithIdentifier:@"MacPDFToolbar"];
    toolbar.delegate = self;
    toolbar.displayMode = NSToolbarDisplayModeIconOnly;
    [window setToolbar:toolbar];

    //NSMutableArray* a = [@[] mutableCopy];
    //[self dumpView:self.contentViewController.view collect:a];
    //[[self window] visualizeConstraints:a];

    _pdfDocument = document;
    return self;
}

- (NSViewController*)viewControllerForView:(NSView*)view
{
    NSViewController* view_controller = [[NSViewController alloc] initWithNibName:nil bundle:nil];
    view_controller.view = view;
    return view_controller;
}

- (NSSplitViewItem*)makeSidebarSplitItem
{
    // FIXME: janky cursor outline
    // FIXME: janky vertical offset of highlight

    side_view = [[NSOutlineView alloc] initWithFrame:NSZeroRect];

    side_view.style = NSTableViewStyleSourceList;
    side_view.focusRingType = NSFocusRingTypeNone;

    //side_view.autoresizingMask = NSViewWidthSizable | NSViewHeightSizable;
    //side_view.translatesAutoresizingMaskIntoConstraints = NO;
    //side_view.autoresizingMask = NSViewWidthSizable | NSViewHeightSizable | NSViewMinYMargin | NSViewMaxXMargin;
    //side_view.autoresizingMask = NSViewWidthSizable | NSViewHeightSizable | NSViewMaxYMargin | NSViewMaxXMargin;

    NSTableColumn* column = [[NSTableColumn alloc] initWithIdentifier:@"col"];
    column.editable = NO;
    [side_view addTableColumn:column];

NSView *view = [[NSView alloc] initWithFrame:NSZeroRect];
//NSVisualEffectView* view = [[NSVisualEffectView alloc] initWithFrame:NSZeroRect];
//view.blendingMode = NSVisualEffectBlendingModeBehindWindow;
//view.material = NSVisualEffectMaterialSidebar;
//view.material = NSVisualEffectMaterialUnderWindowBackground;
//view.material = NSVisualEffectMaterialHUDWindow;

#if 1
// FIXME: need a scroller, but this code here hides the outline
NSScrollView *scrollView = [[NSScrollView alloc] initWithFrame:NSZeroRect];
scrollView.hasVerticalScroller = YES;
scrollView.documentView = side_view;

[view addSubview:scrollView];

[scrollView.topAnchor constraintEqualToAnchor:view.safeAreaLayoutGuide.topAnchor].active = YES;
[scrollView.leftAnchor constraintEqualToAnchor:view.safeAreaLayoutGuide.leftAnchor].active = YES;
[scrollView.rightAnchor constraintEqualToAnchor:view.safeAreaLayoutGuide.rightAnchor].active = YES;
[scrollView.bottomAnchor constraintEqualToAnchor:view.safeAreaLayoutGuide.bottomAnchor].active = YES;
scrollView.translatesAutoresizingMaskIntoConstraints = NO;
#else
[view addSubview:side_view];
#endif

    NSSplitViewItem* item = [NSSplitViewItem sidebarWithViewController:[self viewControllerForView:view]];
    //NSSplitViewItem* item = [NSSplitViewItem sidebarWithViewController:[self viewControllerForView:side_view]];
    item.collapseBehavior = NSSplitViewItemCollapseBehaviorPreferResizingSplitViewWithFixedSiblings;

    // This only has an effect on the very first run.
    // Later, the collapsed state is loaded from the sidebar's autosave data.
    item.collapsed = YES;

    //item.holdingPriority = NSLayoutPriorityDefaultHigh;
    return item;
}

- (void)pdfDidInitialize
{
    [_pdfView setDocument:_pdfDocument.pdf->make_weak_ptr()];
    [self pageChanged];

    // XXX reloadData implied by setDataSource
    // XXX is there a way to only compute this if the sidebar is actually shown?
    _outlineDataSource = [[MacPDFOutlineViewDataSource alloc] initWithOutline:_pdfDocument.pdf->outline()];
    side_view.dataSource = _outlineDataSource;

    side_view.delegate = self;

NSLog(@"outline intrinsic size %@", NSStringFromSize([side_view intrinsicContentSize]));
}

- (IBAction)showGoToPageDialog:(id)sender
{
    auto alert = [[NSAlert alloc] init];
    alert.messageText = @"Page Number";
    [alert addButtonWithTitle:@"Go"];
    [alert addButtonWithTitle:@"Cancel"];

    auto textField = [[NSTextField alloc] initWithFrame:NSMakeRect(0, 0, 100, 24)];
    NSNumberFormatter* formatter = [[NSNumberFormatter alloc] init];
    formatter.numberStyle = NSNumberFormatterNoStyle; // Integers only.
    [textField setFormatter:formatter];
    [textField setIntValue:[_pdfView page]];

    alert.accessoryView = textField;
    alert.window.initialFirstResponder = textField;

    [alert beginSheetModalForWindow:self.window
                  completionHandler:^(NSModalResponse response) {
                      if (response == NSAlertFirstButtonReturn)
                          [self->_pdfView goToPage:[textField intValue]];
                  }];
}

#pragma mark - MacPDFViewDelegate

- (void)pageChanged
{
    [self.window setSubtitle:
                     [NSString stringWithFormat:@"Page %d of %d", [_pdfView page], _pdfDocument.pdf->get_page_count()]];
}

#pragma mark - NSToolbarDelegate

- (NSArray<NSToolbarItemIdentifier>*)toolbarAllowedItemIdentifiers:(NSToolbar*)toolbar
{
    return [self toolbarDefaultItemIdentifiers:toolbar];
}

- (NSArray<NSToolbarItemIdentifier>*)toolbarDefaultItemIdentifiers:(NSToolbar*)toolbar
{
    // NSToolbarToggleSidebarItemIdentifier sends toggleSidebar: along the responder chain,
    // which NSSplitViewController conveniently implements.
    return @[
        NSToolbarToggleSidebarItemIdentifier,
        NSToolbarSidebarTrackingSeparatorItemIdentifier,
    ];
}

- (NSToolbarItem*)toolbar:(NSToolbar*)toolbar
        itemForItemIdentifier:(NSToolbarItemIdentifier)itemIdentifier
    willBeInsertedIntoToolbar:(BOOL)flag
{
    // Not called for standard identifiers, but the implementation of the method must exist, or else:
    // ERROR: invalid delegate <MacPDFWindowController: 0x600003054c80> (does not implement all required methods)
    return nil;
}

#pragma mark - NSOutlineViewDelegate

- (void)outlineViewSelectionDidChange:(NSNotification *)notification
{
    NSInteger row = side_view.selectedRow;
    if (row == -1)
        return;

    OutlineItemWrapper *item = [side_view itemAtRow:row];
    if (item->_item->dest.page.has_value())
        [_pdfView goToPage:item->_item->dest.page.value() + 1];
}

@end
