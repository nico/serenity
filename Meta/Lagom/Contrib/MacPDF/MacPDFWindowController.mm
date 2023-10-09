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

    NSSplitViewController* split_view = [[NSSplitViewController alloc] initWithNibName:nil bundle:nil];
    [split_view addSplitViewItem:[self makeSidebarSplitItem]];
    [split_view addSplitViewItem:[NSSplitViewItem splitViewItemWithViewController:[self viewControllerForView:_pdfView]]];

    // Autosave if the sidebar is open or not, and how far.
    // autosaveName only works if identifier is set too.
    // identifier docs: "For programmatically created views, you typically set this value
    // after creating the item but before adding it to a window. [...] For views and controls
    // in a window, the value you specify for this string must be unique on a per-window basis."
    split_view.splitView.autosaveName = @"MacPDFSplitView";
    split_view.splitView.identifier = @"MacPDFSplitViewId";

    split_view.splitView.vertical = YES; // XXX needed per doc, but not in practice?

    window.contentViewController = split_view;

    NSToolbar* toolbar = [[NSToolbar alloc] initWithIdentifier:@"MacPDFToolbar"];
    toolbar.delegate = self;
    toolbar.displayMode = NSToolbarDisplayModeIconOnly;
    [window setToolbar:toolbar];

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
    side_view = [[NSOutlineView alloc] initWithFrame:NSZeroRect];

    side_view.style = NSTableViewStyleSourceList;
    side_view.focusRingType = NSFocusRingTypeNone;
    side_view.headerView = nil; // breaks background (??) => fixed by setting scrollView.drawsBackground = NO :^)
    //side_view.cornerView = nil; // do i want this?

    //side_view.allowsColumnReordering = NO;

    side_view.floatsGroupRows = NO; // Prevent sticky header (XXX: needed?)
    //side_view.controlSize = NSControlSizeSmall; // XXX no effect
    //side_view.controlSize = NSControlSizeMini; // XXX no effect
    //side_view.autosaveName = @"MacPDFOutlineView";
    //side_view.autosaveExpandedItems = YES;// XXX requires outlineView:itemForPersistentObject: / outlineView:persistentObjectForItem:

    // XXX important. with this set, vertical alignment is better.
    // This defaults to NSTableViewRowSizeStyleCustom (!) which is wrong for source lists.
    side_view.rowSizeStyle = NSTableViewRowSizeStyleDefault;
    //side_view.rowSizeStyle = NSTableViewRowSizeStyleLarge; // makes row height larger, but doesn't affect text. this is the row height that preview uses.
    //side_view.rowSizeStyle = NSTableViewRowSizeStyleSmall; // does make row height smaller, but not text

    NSTableColumn* column = [[NSTableColumn alloc] initWithIdentifier:@"col"];
    column.editable = NO;
    [side_view addTableColumn:column];

    NSView *view = [[NSView alloc] initWithFrame:NSZeroRect];

    // FIXME: need a scroller, but this code here hides the outline
    NSScrollView *scrollView = [[NSScrollView alloc] initWithFrame:NSZeroRect];
    scrollView.hasVerticalScroller = YES;
    scrollView.drawsBackground = NO;
    scrollView.documentView = side_view;

    [view addSubview:scrollView];

    [scrollView.topAnchor constraintEqualToAnchor:view.safeAreaLayoutGuide.topAnchor].active = YES;
    [scrollView.leftAnchor constraintEqualToAnchor:view.safeAreaLayoutGuide.leftAnchor].active = YES;
    [scrollView.rightAnchor constraintEqualToAnchor:view.safeAreaLayoutGuide.rightAnchor].active = YES;
    [scrollView.bottomAnchor constraintEqualToAnchor:view.safeAreaLayoutGuide.bottomAnchor].active = YES;
    scrollView.translatesAutoresizingMaskIntoConstraints = NO;

    NSSplitViewItem* item = [NSSplitViewItem sidebarWithViewController:[self viewControllerForView:view]];
    item.collapseBehavior = NSSplitViewItemCollapseBehaviorPreferResizingSplitViewWithFixedSiblings;

    // This only has an effect on the very first run.
    // Later, the collapsed state is loaded from the sidebar's autosave data.
    item.collapsed = YES;

    return item;
}

- (void)pdfDidInitialize
{
    [_pdfView setDocument:_pdfDocument.pdf->make_weak_ptr()];
    [self pageChanged];

    // XXX is there a way to only compute this if the sidebar is actually shown?
    _outlineDataSource = [[MacPDFOutlineViewDataSource alloc] initWithOutline:_pdfDocument.pdf->outline()];
    side_view.dataSource = _outlineDataSource;
    side_view.delegate = self;
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

- (BOOL)outlineView:(NSOutlineView *)outlineView shouldSelectItem:(id)item
{
    return ![self outlineView:outlineView isGroupItem:item];
}

- (void)outlineViewSelectionDidChange:(NSNotification *)notification
{
    NSInteger row = side_view.selectedRow;
    if (row == -1)
        return;

    OutlineItemWrapper *item = [side_view itemAtRow:row];
    if (auto page = [item page]; page.has_value())
        [_pdfView goToPage:page.value()];
}

- (BOOL)outlineView:(NSOutlineView *)outlineView isGroupItem:(id)item
{
    return [item isGroupItem];
}

// "This method is required if you wish to turn on the use of NSViews instead of NSCells."
- (NSView *)outlineView:(NSOutlineView *)outlineView viewForTableColumn:(NSTableColumn *)tableColumn item:(id)item
{
    // "'tableColumn' will be nil if the row is a group row."

    // The implementation of this method will usually call -[tableView makeViewWithIdentifier:[tableColumn identifier] owner:self]
    // in order to reuse a previous view, or automatically unarchive an associated prototype view for that identifier.

    NSTableCellView* v = [outlineView makeViewWithIdentifier:tableColumn.identifier owner:self];

    // Needed if the cell view isn't loaded from a nib
    if (!v) {
        // XXX comment on `v.identifier`
        v = [[NSTableCellView alloc] init];
        v.identifier = tableColumn.identifier;

        NSTextField* tf = [NSTextField labelWithString:@""];
        tf.lineBreakMode = NSLineBreakByTruncatingTail;

        //tf.controlSize = NSControlSizeMini;  // XXX no effect :/

        // https://stackoverflow.com/a/29725553/551986
        // "If your cell view is an NSTableCellView, that class also responds to -setObjectValue:. [...]
        //  However, an NSTableCellView does not inherently do anything with the object value. It just holds it.
        //  What you can then do is have the subviews bind to it through the objectValue property."
        [tf bind:@"objectValue" toObject:v withKeyPath:@"objectValue" options:nil];

        [v addSubview:tf];
        v.textField = tf;
    }

    return v;
}

@end
