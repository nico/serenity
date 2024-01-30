/*
 * Copyright (c) 2023, Nico Weber <thakis@chromium.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#import "AppDelegate.h"

#include <LibCore/ResourceImplementationFile.h>
#include <LibGfx/ICC/Tags.h>
#include <LibPDF/ColorSpace.h>

@interface AppDelegate ()
{
    NSData* _iccData;
}
@property (strong) IBOutlet NSWindow* window;
@end

@implementation AppDelegate

- (void)applicationDidFinishLaunching:(NSNotification*)aNotification
{
    // FIXME: Copy fonts and icc file to the bundle or something

    // Get from `Build/lagom/bin/MacPDF.app/Contents/MacOS/MacPDF` to `.`.
    NSString* source_root = [[NSBundle mainBundle] executablePath];
    for (int i = 0; i < 7; ++i)
        source_root = [source_root stringByDeletingLastPathComponent];
    auto source_root_string = ByteString([source_root UTF8String]);
    Core::ResourceImplementation::install(make<Core::ResourceImplementationFile>(MUST(String::formatted("{}/Base/res", source_root_string))));

    // Get from ``Build/lagom/bin/MacPDF.app/Contents/MacOS/MacPDF` to `Build/lagom`.
    NSString* build_root = [[NSBundle mainBundle] executablePath];
    for (int i = 0; i < 5; ++i)
        build_root = [build_root stringByDeletingLastPathComponent];
    auto build_root_string = ByteString([build_root UTF8String]);
    Core::ResourceImplementation::install(make<Core::ResourceImplementationFile>(MUST(String::formatted("{}/Userland/Libraries/LibPDF", build_root_string))));
}

- (void)applicationWillTerminate:(NSNotification*)aNotification
{
}

- (BOOL)applicationSupportsSecureRestorableState:(NSApplication*)app
{
    return YES;
}

- (BOOL)application:(NSApplication*)sender openFile:(NSString*)filename
{
    [[NSDocumentController sharedDocumentController]
        openDocumentWithContentsOfURL:[NSURL fileURLWithPath:filename]
                              display:YES
                    completionHandler:^(NSDocument*, BOOL, NSError*) {}];
    return YES;
}

@end
