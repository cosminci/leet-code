SELECT t.request_at AS "Day",
    ROUND(COUNT(
        CASE WHEN t.status != 'completed' THEN 1 ELSE NULL END
        ) / COUNT(id), 2
    ) AS "Cancellation Rate"
FROM trips AS t
JOIN users AS client ON t.client_id = client.users_id AND client.banned = 'No'
JOIN users AS driver ON driver.users_id = t.driver_id AND driver.banned = 'No'
AND t.request_at BETWEEN '2013-10-01' AND '2013-10-03'
GROUP BY t.request_at;
