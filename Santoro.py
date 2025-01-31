
"""
Programming and Algorithm in Python
Gianmarco Santoro - MD2SL
"""


# Import library to calculate execution time of function
import time as chrono


# Get the GLOBAL starting time
global_start_time = chrono.time()


def calc_fun(element_dict, element, value):
    """
    This function updates the key/values in the dictionary
    """

    # If element (primate/couple) in dict
    if element in element_dict:

        # Add a new value for that element
        if value not in element_dict[element]:
            element_dict[element].append(value)

    # If element not in dict add as a new key and the value considered
    else:
        element_dict[element] = [value]

    # Return the updated dict
    return element_dict


def count_elem(element_dict, element, duration=None, line_data=None, interactions_days=None):
    """
    For each line passed of the considered file:
    Fill the dict with element in counting if not already added to dict create a new key
    and assign a 0 + 1 else + duration
    """

    # If counting interactions
    if interactions_days:

        # Extract the day of the current line
        day = timer_fun(line_data[3].strip())

        calc_fun(element_dict, element, day)

    # If counting duration
    elif duration:

        # Extract duration of the current line and add to element in counting
        element_dict[element] = element_dict.get(element, 0) + int(line_data[5])

    # If counting records
    else:

        # Increment the value of the element. If the element-key does not exist it creates and add 1
        element_dict[element] = element_dict.get(element, 0) + 1

    # Return the updated dictionary
    return element_dict


def couple_fun(line_data):
    """
    This functions just create couple-element made by Actor and Recipient in ascending order to catch cases in which
    elements of the couple are the same but roles are switched, so that it counts as the same couple
    """

    # Extract Actor and Recipient from the current file-line
    actor = line_data[1]
    recipient = line_data[2]

    # Tuple so that is immutable, sort to consider, in both direction of action, the same pair of primates
    element = tuple(sorted([actor, recipient]))

    # Return the element made by the couple
    return element


def timer_fun(element, time="date", precision=None):
    """
    This function returns the needed part of the timestamp
    """

    # If the day is needed
    if time == "date":
        # Split the timestamp string by whitespace to separate date-time and take only the date
        element = element.split()[0]

    # Extract hour
    else:
        # Split the timestamp string by whitespace to separate date-time and take only the hour
        element = element.split()[1]

        # If the minutes in the hour aren't important, considering only the hour
        if precision == "hour":
            element = element.split(':')[0]

    # Return the clean timestamp
    return element


def max_fun(element_dict):
    """
    This function finds and returns max values
    """

    # Calculate the max value of the dict
    max_elem = max(len(elem) for elem in element_dict.values())

    # Create a list of the keys from dict if values are max. -.items return key-values to (key, elem)-
    keys_max_elem = [key for key, elem in element_dict.items() if len(elem) == max_elem]

    return keys_max_elem, max_elem


def main_fun(answer=None,
             key_elem=None,
             time="date",
             precision="hour",
             couple=None,
             duration=None,
             behavior=None,
             file='OBS_data.txt',
             interactions_days=None):
    """
    :return: just manage the query and print the results
    """

    # Get the starting time
    start_time = chrono.time()

    # Create dicts and list to be filled
    col_dict = {}       # Where names of the file-columns are stored
    element_dict = {}   # Dict that adapts to the query

    # Open the file where data are stored
    with open(file, 'r') as openfile:

        # Columns names extracted by the first line of the txt file with readline method and assign to a list,
        # strip is to avoid whitespaces and split to select the separator (tab, in this case)
        col_list = openfile.readline().strip().split('\t')

        # Create a dict with names of columns as keys and position numbers as values
        for index, key in enumerate(col_list):
            col_dict[key] = index

        # For each line of the file
        for line in openfile:

            # Process each line has tab-separated values
            line_data = line.strip().split('\t')

            # If query requires behaviour analysis
            if behavior:

                # Extract the behavior of the record
                behave = line_data[3].strip()

                # If query requires time analysis
                if key_elem == "DateTime":

                    # Assign to element the timestamp
                    element = line_data[0]

                    # Elaborate the timestamp as needed
                    element = timer_fun(element, time, precision)

                    # Count behaviours and update the dictionary
                    element_dict = calc_fun(element_dict, element, behave)

                # If query requires to count by couples
                elif couple:

                    # Check if there is both Actor and Recipient
                    if line_data[1] and line_data[2]:

                        # Create element by couple
                        element = couple_fun(line_data)

                        # Count behaviours and update the dictionary
                        element_dict = calc_fun(element_dict, element, behave)

                # If query requires single primates
                else:

                    # Take the Actor from record
                    actor = line_data[1].strip()

                    # Count behaviours of the Actor and update the dictionary
                    element_dict = calc_fun(element_dict, actor, behave)

                    # Check if there is Recipient in the record
                    if str(line_data[2]) != '':

                        # Extract the Recipient from the record
                        recipient = line_data[2].strip()

                        # Count behaviours of the Recipient and update the dictionary
                        element_dict = calc_fun(element_dict, recipient, behave)

            # If query requires not behaviour but couple records counting
            elif couple:

                # Check if both Actor and Recipient are in the record, if not just go to the next line
                if line_data[1] and line_data[2]:

                    # Create element by couple
                    element = couple_fun(line_data)

                    # Increment the value of the pair. If the pair-key does not exist it creates and add 1
                    element_dict = count_elem(element_dict, element, duration, line_data, interactions_days)

            # If query requires single primate records counting
            else:

                # For each column to be considered, e.g.Actor, or Actor and Recipient, ...
                for elem in key_elem:

                    # Takes the indices of the corresponding
                    num = col_dict[elem]

                    # Assign to element to simplify notation
                    element = line_data[num]

                    # If there is a value
                    if element:

                        # If query need DateTime
                        if "DateTime" in key_elem:

                            # Take the date/time needed
                            element = timer_fun(element, time, precision)

                        # Update the dictionary
                        element_dict = count_elem(element_dict, element, duration, line_data, interactions_days)

    # Print the outcome of the analysis required by the query
    print(f"---------------------------------------------------\n"
          f"Question {answer}, answer:")

    # If query requires behaviour count
    if behavior:

        # Function to calculate max values and corresponding keys
        keys_max_elem, max_elem = max_fun(element_dict)

        # For each max-key
        for key in keys_max_elem:
            print(f"\t{'Day' if key_elem == 'DateTime' else 'Primate'}"
                  f"{' couple' if couple else ''} with most different behavior is: {key} "
                  f"with {max_elem} different behaviours\n")

    # If not required behaviour
    else:

        # If working on interactions
        if file == "RFID_data.txt":

            # If query requires days with interactions count
            if interactions_days:

                # Function to calculate max values and corresponding keys
                keys_max_elem, max_elem = max_fun(element_dict)

                # Print how many max values and with how many days of interactions
                print(f"\tThere are {len(keys_max_elem)} primate{' couples' if couple else 's'} "
                      f"with {max_elem} interaction days")

                # Print names of primates
                print(f"\tPrimate{' couples' if couple else ''} with most interaction days are:")

                # Enumerate to iterate and take into account index of iteration, 1 to specify the index to start from 1
                for i, key in enumerate(keys_max_elem, 1):
                    print(f"\t{key}\t", end="")  # "end" to specify to tab and not default = newline

                    # Every 3 iteration print in a new line, just to improve visual format readability
                    if i % 3 == 0:
                        print("\n", end="")
                print("\n")

            # If it doesn't require days count
            else:
                print(f"\t{'Day' if 'DateTime' in key_elem else 'Primate'}"
                      f"{' couple' if couple else ''} with most interactions is: "
                      
                      # Find the max value in the dict
                      f"{max(element_dict, key=element_dict.get)}, with {max(element_dict.values())} interactions")

        # If counted primates doing things
        else:
            print(f"\tThe {' & '.join(elem for elem in key_elem)}"  # Here to specify element/s counted
                  f"{'-' + time if 'DateTime' in key_elem else ''}"
                  f"{', considering a precision of ' + precision + ',' if time != 'date' else ''}"
                  f" with highest {'time ' if duration else ''}involvement{' as couple' if couple else ''} is: "
                  
                  # Find the max value in the dict
                  f"{max(element_dict, key=element_dict.get)}, with {max(element_dict.values())} "
                  f"{'seconds' if duration else 'events'}")

    # Time to read the file, create dictionary based on query, max research and print
    end_time = chrono.time()

    # Time to execute the function
    time_global = end_time - start_time

    # Display times
    print(f"\tFunction time: {time_global:.6f} s\n")


# ---- MAIN ----

# ---- Part A ----

"""
The data set contains observational and wearable sensors data collected in a group of 20 Guinea baboons living in an
enclosure of a Primate Center in France, between June 13th 2019 and July 10th 2019.

A) OBS DATA

The file OBS_data.txt contains all the behavioral events registered by an observer, with 7 columns:
– DateTime = Time stamp of the event, namely the moment the observed behavior was registered. In case of STATE events
             (events with duration > 0), it refers to the beginning of the behavior;
– Actor = The name of the actor;
– Recipient = The name of the individual the Actor is acting upon;
– Behavior = The behavior the Actor. 14 types of behaviors are registered:’Resting’, ‘Grooming’, ‘Presenting’,
             ’Playing with’, ‘Grunting-Lipsmacking’, ‘Supplanting’,’Threatening’, ‘Submission’, ‘Touching’,
             ‘Avoiding’, ‘Attacking’,’Carrying’, ‘Embracing’, ‘Mounting’, ‘Copulating’, ‘Chasing’.
             In addition two other categories were included: ‘Invisible’ and ‘Other’;
– Category = The classification of the behavior. It can be ‘Affiliative’, ‘Agonistic’, ‘Other’;
– Duration = Duration of the observed behavior. POINT events have no duration;
– Point = indicates if the event is a POINT event (YES) or a STATE event (NO).
"""

# 1) Qual è il primate che è stato coinvolto in più eventi sia come "Actor" che come "Recipient"?
main_fun(answer="1", key_elem=["Actor", "Recipient"])

# 2) Qual è il primate che è stato coinvolto in più eventi come "Actor"?
main_fun(answer="2", key_elem=["Actor"])

# 3) Qual è il primate che è stato coinvolto in più eventi come "Recipient"?
main_fun(answer="3", key_elem=["Recipient"])

# 4) Qual è il giorno in cui ci sono stati più eventi?
main_fun(answer="4", key_elem=["DateTime"])

# 5a) Qual è l'ora del giorno in cui ci sono più eventi?
#     Considerando solo l'ora
main_fun(answer="5a", key_elem=["DateTime"], time="hour")

# 5b) Qual è l'ora del giorno in cui ci sono più eventi?
#     Considerando l'ora e il minuto
main_fun(answer="5b", key_elem=["DateTime"], time="hour", precision="min")

# 6) Qual è il tipo di comportamento (Behavior) maggiormente registrato?
main_fun(answer="6", key_elem=["Behavior"])

# 7) Qual è la coppia di primati coinvolta insieme in più eventi?
main_fun(answer="7", key_elem=["Actor", "Recipient"], couple=True)

# 8) Qual è il primate che è stato coinvolto più a lungo in eventi sia come "Actor" che come "Recipient"?
#    (contando le durate)
main_fun(answer="8", key_elem=["Actor", "Recipient"], duration=True)

# 9) Qual è il primate che è stato coinvolto più a lungo in eventi come "Actor"? (contando le durate)
main_fun(answer="9", key_elem=["Actor"], duration=True)

# 10) Qual è il primate che è stato coinvolto più a lungo in eventi come "Recipient"? (contando le durate)
main_fun(answer="10", key_elem=["Recipient"], duration=True)

# 11) Qual è la coppia di primati coinvolta insieme più a lungo (in più eventi contando le durate)?
main_fun(answer="11", key_elem=["Actor", "Recipient"], couple=True, duration=True)

# 12) Qual è il primate con più comportamenti diversi?
#  Se A è coinvolto due volte nel comportamento "Playing with", questo conta una sola volta.
main_fun(answer="12", key_elem=["Actor", "Recipient"], behavior=True)

# 13) Qual è la coppia di primati coinvolta insieme in più eventi diversi?
#  Se A e B sono coinvolti due volte nel comportamento "Playing with", questo conta una sola volta
main_fun(answer="13", couple=True, behavior=True)

# 14) Qual è il giorno con più comportamenti diversi?
#  Se in un giorno compare due volte il comportamento "Playing with", questo conta una sola volta.
main_fun(answer="14", key_elem="DateTime", behavior=True)


# ---- Part B ----

"""
B) SENSOR DATA

The file RFID_data.txt contains contacts data recorded in the same period by the SocioPatterns infrastructure.
The proximity sensors were worn by 13 of the 20 individuals cited above.
The data file consists of 4 columns:
– t = time of the beginning of the contact in Epoch format (Unix timestamps);
– i = Name of the first individual;
– j = Name of the second individual;
– DateTime
"""

# 15) Qual è la coppia di primati coinvolta insieme in più interazioni?
main_fun(answer="15", key_elem=["i", "j"], couple=True, file='RFID_data.txt')

# 16) Qual è il primate coinvolto in più interazioni?
main_fun(answer="16", key_elem=["i", "j"], file='RFID_data.txt')

# 17) Qual è il primate che ha interazioni in più giorni? Se in un giorno ha più interazioni, il giorno conta uno.
main_fun(answer="17", key_elem=["i", "j"], file='RFID_data.txt', interactions_days=True)

# 18) Qual è la coppia di primati che ha interazioni in più giorni?
#  Se in un giorno hanno più interazioni, il giorno conta uno.
main_fun(answer="18", key_elem=["i", "j"], couple=True, file='RFID_data.txt', interactions_days=True)

# 19) Qual è il giorno con più interazioni?
main_fun(answer="19", key_elem=["DateTime"], file='RFID_data.txt')


# ----
# Overall time to run the whole script
print(f"---------------------------------------------------\n")

# Time at end of all processes
global_end_time = chrono.time()

# DeltaTime to read the file and create dictionary based on query
global_time = global_end_time - global_start_time

# Display Global overall time
print(f"Global time to run the whole script: {global_time:.6f} seconds\n")
