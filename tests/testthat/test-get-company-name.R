describe("get_company_name()", {

  it("errors when company_id is missing", {
    expect_error(get_company_name(), "company_id")
  })

  it("errors when company_id is not a character", {
    expect_error(get_company_name(123), "company_id")
  })
})


describe("get_franchise_name()", {

  it("errors when franchise_id is missing", {
    expect_error(get_franchise_name(), "franchise_id")
  })

  it("errors when franchise_id is not a character", {
    expect_error(get_franchise_name(123), "franchise_id")
  })
})
