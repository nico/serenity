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
    // FIXME: Copy the fonts to the bundle or something

    // Get from `Build/lagom/bin/MacPDF.app/Contents/MacOS/MacPDF` to `.`.
    NSString* source_root = [[NSBundle mainBundle] executablePath];
    for (int i = 0; i < 7; ++i)
        source_root = [source_root stringByDeletingLastPathComponent];
    auto source_root_string = ByteString([source_root UTF8String]);
    Core::ResourceImplementation::install(make<Core::ResourceImplementationFile>(MUST(String::formatted("{}/Base/res", source_root_string))));

    // XXX reentrancy
    PDF::DeviceCMYKColorSpace::set_default_cmyk_profile_loader([&self]() {
        NSString* path = [[NSBundle mainBundle] pathForResource:@"USWebCoatedSWOP" ofType:@"icc" inDirectory:@"Adobe/CMYK"];
        NSLog(@"attempting to load CMYK profile from %@", path);
        _iccData = [NSData dataWithContentsOfFile:path];
        ReadonlyBytes icc_data = { _iccData.bytes, (size_t)_iccData.length };
        auto profile = MUST(Gfx::ICC::Profile::try_load_from_externally_owned_memory(icc_data));
        dbgln("desc: {}", profile->tag_string_data(Gfx::ICC::profileDescriptionTag));
        return profile;
    });
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
