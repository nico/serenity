include(${CMAKE_CURRENT_LIST_DIR}/utils.cmake)

if (ENABLE_ADOBE_ICC_PROFILES_DOWNLOAD)
    set(ADOBE_ICC_PROFILES_PATH "${SERENITY_CACHE_DIR}/AdobeICCProfiles" CACHE PATH "Download location for Adobe ICC profiles")
    set(ADOBE_ICC_PROFILES_DATA_URL "https://download.adobe.com/pub/adobe/iccprofiles/win/AdobeICCProfilesCS4Win_end-user.zip")
    set(ADOBE_ICC_PROFILES_ZIP_PATH "${ADOBE_ICC_PROFILES_PATH}/adobe-icc-profiles.zip")

    function(extract_adobe_icc_profiles source path)
        if(EXISTS "${ADOBE_ICC_PROFILES_ZIP_PATH}" AND NOT EXISTS "${path}")
            message(STATUS "Extracting using cmake ${source}")
            file(ARCHIVE_EXTRACT INPUT "${ADOBE_ICC_PROFILES_ZIP_PATH}" DESTINATION "${ADOBE_ICC_PROFILES_PATH}" PATTERNS "${source}")
        endif()
    endfunction()


    download_file("${ADOBE_ICC_PROFILES_DATA_URL}" "${ADOBE_ICC_PROFILES_ZIP_PATH}")

    set(ADOBE_ICC_CMYK_SWOP "CMYK/USWebCoatedSWOP.icc")
    set(ADOBE_ICC_CMYK_SWOP_PATH "${ADOBE_ICC_PROFILES_PATH}/Adobe ICC Profiles (end-user)/${ADOBE_ICC_CMYK_SWOP}")
    extract_adobe_icc_profiles("Adobe ICC Profiles (end-user)/${ADOBE_ICC_CMYK_SWOP}" "${ADOBE_ICC_CMYK_SWOP_PATH}")

    set(ADOBE_ICC_PROFILES_INSTALL_PATH "${CMAKE_CURRENT_BINARY_DIR}/icc/Adobe")
    set(ADOBE_ICC_CMYK_SWOP_INSTALL_PATH "${ADOBE_ICC_PROFILES_INSTALL_PATH}/${ADOBE_ICC_CMYK_SWOP}")
    configure_file("${ADOBE_ICC_CMYK_SWOP_PATH}" "${ADOBE_ICC_CMYK_SWOP_INSTALL_PATH}" COPYONLY)

    if (SERENITYOS)
        install(DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/icc/Adobe/" DESTINATION "${CMAKE_BINARY_DIR}/Root/res/icc/Adobe")
    endif()
endif()
