# Integration tests for API lookup functions
# These tests require a valid FLIX_PATROL API key

skip_if_no_api_key <- function() {
    key <- Sys.getenv("FLIX_PATROL")
    if (nchar(key) < 1) {
        skip("FLIX_PATROL API key not set")
    }
}

describe("lookup_country()", {

    it("returns a character ID for a valid country", {
        skip_if_no_api_key()
        result <- lookup_country("United States", silent = TRUE)
        expect_type(result, "character")
        expect_true(nchar(result) > 0)
    })

    it("works with multiple countries", {
        skip_if_no_api_key()
        result <- lookup_country(c("United States", "Germany"), silent = TRUE)
        expect_type(result, "character")
        expect_length(result, 2)
    })

    it("errors for non-existent country", {
        skip_if_no_api_key()
        expect_error(lookup_country("Fakelandia", silent = TRUE))
    })
})

describe("lookup_platform()", {

    it("returns a character ID for Netflix", {
        skip_if_no_api_key()
        result <- lookup_platform("Netflix", silent = TRUE)
        expect_type(result, "character")
        expect_true(grepl("^cmp_", result))
    })

    it("works with multiple platforms", {
        skip_if_no_api_key()
        result <- lookup_platform(c("Netflix", "Disney+"), silent = TRUE)
        expect_type(result, "character")
        expect_length(result, 2)
    })

    it("errors for non-existent platform", {
        skip_if_no_api_key()
        expect_error(lookup_platform("FakeStreamingService", silent = TRUE))
    })
})

describe("lookup_franchise()", {

    it("returns a character ID for a valid franchise", {
        skip_if_no_api_key()
        result <- lookup_franchise("Indiana Jones", silent = TRUE)
        expect_type(result, "character")
        expect_true(grepl("^frn_", result))
    })

    it("errors for non-existent franchise", {
        skip_if_no_api_key()
        expect_error(lookup_franchise("Fake Franchise XYZ", silent = TRUE))
    })
})

describe("lookup_title()", {

    it("returns a character ID for a valid title", {
        skip_if_no_api_key()
        result <- lookup_title("Squid Game", silent = TRUE)
        expect_type(result, "character")
        expect_true(grepl("^ttl_", result))
    })

    it("errors for non-existent title", {
        skip_if_no_api_key()
        expect_error(lookup_title("Fake Movie Title 12345", silent = TRUE))
    })
})

