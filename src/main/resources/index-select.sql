select company_id, names, ADDDATE(service_date,INTERVAL priority_days DAY) as service_date, priority_days, service_type, service_date as real_service_date from(
    select * from(
        select
            company.company_id,
            company.names,
            service.date1 as service_date,
            company.priority_days,
            service.type1 as service_type
        from company
            join serviceable as parent using (company_id)
            join serviceable as child on (parent.serviceable_id = child.parent_id)
            join service_part on (child.serviceable_id = service_part.serviceable_id)
            join service using (service_id)
            where service.type1 = 'p'
        union
        select
            company.company_id,
            company.names,
            service.date1 as service_date,
            company.priority_days,
            service.type1 as service_type
        from company
            join serviceable as parent using (company_id)
            join service_part on (parent.serviceable_id = service_part.serviceable_id)
            join service using (service_id)
            where service.type1 = 'p'
    ) alias1
    union
    select * from (
        select company_id, names, max(service_date) as service_date, priority_days, service_type from(
            select
                company.company_id,
                company.names,
                ADDDATE(service.date1,INTERVAL child.interval_days DAY) as service_date,
                company.priority_days,
                child.interval_days,
                service.type1 as service_type
            from company
                join serviceable as parent using (company_id)
                join serviceable as child on (parent.serviceable_id = child.parent_id)
                join service_part on (child.serviceable_id = service_part.serviceable_id)
                join service using (service_id)
                where service.type1 = 'n'
                and company.company_id not in (
                    select
                        company.company_id
                    from company
                        join serviceable as parent using (company_id)
                        join serviceable as child on (parent.serviceable_id = child.parent_id)
                        join service_part on (child.serviceable_id = service_part.serviceable_id)
                        join service using (service_id)
                        where service.type1 = 'p'
                    union
                    select
                        company.company_id
                    from company
                        join serviceable as parent using (company_id)
                        join service_part on (parent.serviceable_id = service_part.serviceable_id)
                        join service using (service_id)
                        where service.type1 = 'p'
                )
            union
            select
                company.company_id,
                company.names,
                ADDDATE(service.date1,INTERVAL interval_days DAY) as service_date,
                company.priority_days,
                interval_days,
                service.type1 as service_type
            from company
                join serviceable as parent using (company_id)
                join service_part on (parent.serviceable_id = service_part.serviceable_id)
                join service using (service_id)
                where service.type1 = 'n'
                and company.company_id not in (
                    select
                        company.company_id
                    from company
                        join serviceable as parent using (company_id)
                        join serviceable as child on (parent.serviceable_id = child.parent_id)
                        join service_part on (child.serviceable_id = service_part.serviceable_id)
                        join service using (service_id)
                        where service.type1 = 'p'
                    union
                    select
                        company.company_id
                    from company
                        join serviceable as parent using (company_id)
                        join service_part on (parent.serviceable_id = service_part.serviceable_id)
                        join service using (service_id)
                        where service.type1 = 'p'
                )
            union
            select
                company.company_id,
                company.names,
                ADDDATE(child.into_service,INTERVAL child.interval_days DAY) as service_date,
                company.priority_days,
                child.interval_days,
                NULL as service_type
            from company
                join serviceable as parent using (company_id)
                join serviceable as child on (parent.serviceable_id = child.parent_id)
                where company.company_id not in (
                    select
                        company.company_id
                    from company
                        join serviceable as parent using (company_id)
                        join serviceable as child on (parent.serviceable_id = child.parent_id)
                        join service_part on (child.serviceable_id = service_part.serviceable_id)
                        join service using (service_id)
                        where service.type1 = 'p'
                    union
                    select
                        company.company_id
                    from company
                        join serviceable as parent using (company_id)
                        join service_part on (parent.serviceable_id = service_part.serviceable_id)
                        join service using (service_id)
                        where service.type1 = 'p'
                )
            union
            select
                company.company_id,
                company.names,
                ADDDATE(into_service,INTERVAL interval_days DAY) as service_date,
                company.priority_days,
                interval_days,
                NULL as service_type
            from company
                join serviceable as parent using (company_id)
                where company.company_id not in (
                    select
                        company.company_id
                    from company
                        join serviceable as parent using (company_id)
                        join serviceable as child on (parent.serviceable_id = child.parent_id)
                        join service_part on (child.serviceable_id = service_part.serviceable_id)
                        join service using (service_id)
                        where service.type1 = 'p'
                    union
                    select
                        company.company_id
                    from company
                        join serviceable as parent using (company_id)
                        join service_part on (parent.serviceable_id = service_part.serviceable_id)
                        join service using (service_id)
                        where service.type1 = 'p'
                )
        ) alias2
        group by company_id
    ) alias3
    union
    select company_id, names, NULL, priority_days, NULL from company
    where company_id not in(
        select company_id from(
            select
                company.company_id,
                company.names,
                service.date1 as service_date,
                company.priority_days,
                service.type1 as service_type
            from company
                join serviceable as parent using (company_id)
                join serviceable as child on (parent.serviceable_id = child.parent_id)
                join service_part on (child.serviceable_id = service_part.serviceable_id)
                join service using (service_id)
                where service.type1 = 'p'
            union
            select
                company.company_id,
                company.names,
                service.date1 as service_date,
                company.priority_days,
                service.type1 as service_type
            from company
                join serviceable as parent using (company_id)
                join service_part on (parent.serviceable_id = service_part.serviceable_id)
                join service using (service_id)
                where service.type1 = 'p'
        ) alias1
        union
        select company_id from (
            select company_id, names, max(service_date) as service_date, priority_days, service_type from(
                select
                    company.company_id,
                    company.names,
                    ADDDATE(service.date1,INTERVAL child.interval_days DAY) as service_date,
                    company.priority_days,
                    child.interval_days,
                    service.type1 as service_type
                from company
                    join serviceable as parent using (company_id)
                    join serviceable as child on (parent.serviceable_id = child.parent_id)
                    join service_part on (child.serviceable_id = service_part.serviceable_id)
                    join service using (service_id)
                    where service.type1 = 'n'
                    and company.company_id not in (
                        select
                            company.company_id
                        from company
                            join serviceable as parent using (company_id)
                            join serviceable as child on (parent.serviceable_id = child.parent_id)
                            join service_part on (child.serviceable_id = service_part.serviceable_id)
                            join service using (service_id)
                            where service.type1 = 'p'
                        union
                        select
                            company.company_id
                        from company
                            join serviceable as parent using (company_id)
                            join service_part on (parent.serviceable_id = service_part.serviceable_id)
                            join service using (service_id)
                            where service.type1 = 'p'
                    )
                union
                select
                    company.company_id,
                    company.names,
                    ADDDATE(service.date1,INTERVAL interval_days DAY) as service_date,
                    company.priority_days,
                    interval_days,
                    service.type1 as service_type
                from company
                    join serviceable as parent using (company_id)
                    join service_part on (parent.serviceable_id = service_part.serviceable_id)
                    join service using (service_id)
                    where service.type1 = 'n'
                    and company.company_id not in (
                        select
                            company.company_id
                        from company
                            join serviceable as parent using (company_id)
                            join serviceable as child on (parent.serviceable_id = child.parent_id)
                            join service_part on (child.serviceable_id = service_part.serviceable_id)
                            join service using (service_id)
                            where service.type1 = 'p'
                        union
                        select
                            company.company_id
                        from company
                            join serviceable as parent using (company_id)
                            join service_part on (parent.serviceable_id = service_part.serviceable_id)
                            join service using (service_id)
                            where service.type1 = 'p'
                    )
                union
                select
                    company.company_id,
                    company.names,
                    ADDDATE(child.into_service,INTERVAL child.interval_days DAY) as service_date,
                    company.priority_days,
                    child.interval_days,
                    NULL as service_type
                from company
                    join serviceable as parent using (company_id)
                    join serviceable as child on (parent.serviceable_id = child.parent_id)
                    where company.company_id not in (
                        select
                            company.company_id
                        from company
                            join serviceable as parent using (company_id)
                            join serviceable as child on (parent.serviceable_id = child.parent_id)
                            join service_part on (child.serviceable_id = service_part.serviceable_id)
                            join service using (service_id)
                            where service.type1 = 'p'
                        union
                        select
                            company.company_id
                        from company
                            join serviceable as parent using (company_id)
                            join service_part on (parent.serviceable_id = service_part.serviceable_id)
                            join service using (service_id)
                            where service.type1 = 'p'
                    )
                union
                select
                    company.company_id,
                    company.names,
                    ADDDATE(into_service,INTERVAL interval_days DAY) as service_date,
                    company.priority_days,
                    interval_days,
                    NULL as service_type
                from company
                    join serviceable as parent using (company_id)
                    where company.company_id not in (
                        select
                            company.company_id
                        from company
                            join serviceable as parent using (company_id)
                            join serviceable as child on (parent.serviceable_id = child.parent_id)
                            join service_part on (child.serviceable_id = service_part.serviceable_id)
                            join service using (service_id)
                            where service.type1 = 'p'
                        union
                        select
                            company.company_id
                        from company
                            join serviceable as parent using (company_id)
                            join service_part on (parent.serviceable_id = service_part.serviceable_id)
                            join service using (service_id)
                            where service.type1 = 'p'
                    )
            ) alias2
        group by company_id
        ) alias3
    )
) as t
order by %s
