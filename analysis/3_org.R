# subset all organizational characteristics

# melt/clean organization types
df.typesbase <- meltFactor(df, 'T.', 'OrgTypeCat', 'OrgType')
df.typesbase$OrgType <- revalue(df.typesbase$OrgType, c('Records Information Management' = 'Records Mgmt',
                                                        'IT/System administration' = 'IT'))
# melt/clean parent organization types
df.parentorgsbase <- meltFactor(df, 'PO.', 'ParentOrgCat', 'ParentOrg')
df.parentorgsbase$ParentOrg <- revalue(df.parentorgsbase$ParentOrg, c('Academic/Education' = 'Academic',
                                                                      'Corporate/For-profit' = 'Corporate',
                                                                      'County or Municipality' = 'Local',
                                                                      'Nonprofit Organization/Foundation' = 'Nonprofit'))

# melt/clean staff responsibilities
df.resp <- meltFactor(df, 'R.', 'RespCat', 'Resp')
df.resp$Resp <- revalue(df.resp$Resp, c('Paid full or part-time professional staff with practitioner experience' = 'Dedicated',
                                        'Digital preservation duties assigned to various staff as needed' = 'Split among Staff',
                                        'Digital preservation services provided by an external vendor' = 'Vendor',
                                        'Volunteers (full or part-time)' = 'Volunteer',
                                        'No staff person has digital preservation responsibilities' = 'No Staff'))
df.resp$Resp <- factor(df.resp$Resp, c('Dedicated', 'Split among Staff', 'Vendor', 'Volunteer', 'No Staff'))

# subset all single column org factors
df.org <- select(df, ResponseID, State, Timezone, StaffSize, AidAmount, Importance)

# melt/clean content formats
df.cformat <- meltFactor(df, 'CF.', 'ContentFormatCat', 'ContentFormat')
df.cformat$ContentFormat <- revalue(df.cformat$ContentFormat,
                                    c('Born-digital content created by and for your organization' = 'Born-digital',
                                      'Deposited digital materials managed for other individuals or institutions' = 'Deposited materials', 
                                      'Licensed digital content' = 'Licensed',
                                      'Reformatted material digitized from collections already held' = 'Digitized'))

# melt/clean content types
df.ctypes <- meltFactor(df, 'CT.', 'ContentTypeCat', 'ContentType')
df.ctypes$ContentType <- revalue(df.ctypes$ContentType, c('Architectural, CAD, and other design drawings' = 'CAD',
                                                          'Audiovisual files' = 'AV',
                                                          'Digital image files' = 'Images',
                                                          'Geographic information files' = 'GIS',
                                                          'PDF files' = 'PDF',
                                                          'Research data files' = 'Research data'))

# count responses by organzation type and parent organization
g.orgtype <- facetHist(merge(df.typesbase, df.parentorgsbase), 'OrgType', 'ParentOrg')

# filter organizations and parent organizations to largest members
df.parentorgs <- filter(df.parentorgsbase, ParentOrg %in% c('Academic', 'Nonprofit', 'State'))
df.types <- filter(df.typesbase, OrgType %in% c('Archive', 'Library', 'Museum'))

# compare staff size across organzation types
g.orgstaff <- facetHistDensity(merge(df.types, df.org), 'StaffSize', 'OrgType')

# compare staff size in libraries
g.libstaff <- facetHistDensity(merge(merge(df.types[df.types$OrgType == 'Library', ], df.org), df.parentorgsbase), 'StaffSize', 'ParentOrg')

# count minimum staff represented by libraries
df.aclib.staff <- merge(df.types[df.types$OrgType == 'Library', ],
                        df.parentorgs[df.parentorgs$ParentOrg == 'Academic', ]) %>%
        merge(df.org) %>%
        group_by(StaffSize) %>%
        summarize(count = length(ResponseID)) %>%
        select(count)

libstaff <- floor(sum(df.aclib.staff$count * c(1, 13, 25, 51, 201, 501))/sum(df.aclib.staff$count))

# compare digital preservation responsibility by staff size
g.respstaff <- facetHistDensity(merge(df.resp, df.org), 'Resp', 'StaffSize')

# compare digital preservation responsibility by organization type
g.resporg <- facetHistDensity(merge(df.resp, df.types), 'Resp', 'OrgType')

# compare content type by parent organization
g.ctypeparent <- facetHistDensity(merge(df.parentorgs, df.ctypes), 'ContentType', 'ParentOrg')

# compare content format by parent organization
g.cformparent <- facetHistDensity(merge(df.parentorgs, df.cformat), 'ContentFormat', 'ParentOrg')

# compare content type by organization type
g.ctypeorg <- facetHistDensity(merge(df.types, df.ctypes), 'ContentType', 'OrgType')

# compare content format by organization type
g.cformorg <- facetHistDensity(merge(df.types, df.cformat), 'ContentFormat', 'OrgType')

# compare content type by staff size
g.ctypestaff <- facetHistDensity(merge(df.org, df.ctypes), 'ContentType', 'StaffSize')

# compare content format by staff size
g.cformstaff <- facetHistDensity(merge(df.org, df.cformat), 'ContentFormat', 'StaffSize')

# count aid for training
g.aid <- ggplot(df.org, aes(x = AidAmount)) + geom_histogram(fill = '#B34F89')

# filter "don't know"s and least used responses
df.orgaid <- filter(df.org, AidAmount %in% c('$0-250', '$251-500', '$501-750', '$751-1000', '$1001-3000'))

# compare aid to digital preservation responsibility
g.aidresp <- facetHistDensity(merge(df.resp, df.orgaid), 'AidAmount', 'Resp')

# compare aid to staff size
g.aidstaff <- facetHistDensity(merge(df.types, df.orgaid), 'AidAmount', 'StaffSize')