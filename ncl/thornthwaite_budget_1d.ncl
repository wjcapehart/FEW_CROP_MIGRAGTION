

undef("thornthwaite_budget_1d")

procedure thornthwaite_budget_1d(lat:numeric,        \  ; degrees
                                storage_0:numeric,   \  ; kg m-2
                                temperature:numeric, \  ; degC
                                precip:numeric,      \  ; kg m-2
                                pot_evap:numeric,    \  ; kg m-2
                                evaporation:numeric, \  ; kg m-2
                                storage:numeric,     \  ; kg m-2
                                dstorage:numeric,     \  ; kg m-2 mo-1
                                deficit:numeric,     \  ; kg m-2
                                recharge:numeric,    \  ; kg m-2
                                surplus:numeric      )  ; kg m-2




begin


   nt = dimsizes(temperature)



   pot_evap = thornthwaite(temperature, \
                            lat,         \
                            False,       \
                            0            )

   p_m_pe = precip - pot_evap

   dstorage = precip * 0

   t = 0

   storage(t) = storage_0

   do t = 1, dimsizes(precip)-1


      if (.not.ismissing(pot_evap(t))) then

        if (p_m_pe(t) .le. 0) then

           if (storage(t-1) .le. storage_0) then
              storage(t) = storage(t-1)*exp(p_m_pe(t)/storage_0)
           else
              storage(t) = storage_0*exp(p_m_pe(t)/storage_0)
           end if

           ;storage(t)    = round(storage(t),0)
           dstorage(t)    = storage(t) - storage(t-1)
           evaporation(t) = precip(t) - dstorage(t)
           surplus(t)     = 0
           deficit(t)     = pot_evap(t) - evaporation(t)
           recharge(t)    = 0

        else
           storage(t)     = storage(t-1) + p_m_pe(t)
           ;storage(t)    = round(storage(t),0)

           if (storage(t) .gt. storage_0) then
               storage(t) = storage_0
           end if

           dstorage(t) = storage(t) - storage(t-1)
           evaporation(t) = pot_evap(t)

           if ((storage(t) .eq. storage_0) .and. (dstorage(t) .eq. 0)) then

              deficit(t) = 0.
              recharge(t) = 0.
              surplus(t) = precip(t) - evaporation(t)

           else

               if ((storage(t) .ne. storage_0) .and. (dstorage(t) .ne. 0)) then

                   deficit(t)  = 0.
                   recharge(t) = precip(t) - evaporation(t)
                   surplus(t)  = 0.


               else

                  if ((storage(t) .eq. storage_0) .and. (dstorage(t) .ne. 0)) then

                      deficit(t) = 0.
                      recharge(t) = dstorage(t)
                      surplus(t) = precip(t) - evaporation(t) - recharge(t)

                  end if ;((storage(t) .eq. storage_0) .and. (dstorage(t) .ne. 0))

               end if  ; ((storage(t) .ne. storage_0) .and. (dstorage(t) .ne. 0))


           end if ; ((storage(t) .eq. storage_0) .and. (dstorage(t) .eq. 0))

        end if ; (p_m_pe(t) .le. 0)
      end if  ;  if (.not.ismissing(pot_evap(t))) then

   end do



end
